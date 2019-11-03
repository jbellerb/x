use std::fs::File;
use std::io;
use std::io::{Read, Write};
use std::os::unix::io::{AsRawFd, FromRawFd, RawFd};
use std::time::Duration;

use mio::unix::EventedFd;
use mio::{Events, Poll, PollOpt, Ready, Token};
use termios::Termios;
use unic_segment::Graphemes;

const VERSION: &'static str = env!("CARGO_PKG_VERSION");

/*** data ***/

struct Editor {
    fd: RawFd,
    stdin: File,
    termios: Termios,
    poll: Poll,
    cx: i32,
    cy: i32,
    screen_rows: i32,
    screen_cols: i32,
}

#[derive(Clone, Copy)]
enum EditorKey {
    ArrowLeft,
    ArrowRight,
    ArrowUp,
    ArrowDown,
    Delete,
    PageUp,
    PageDown,
    Home,
    End,
    Key(u8),
}

/*** terminal ***/

impl Editor {
    fn from(fd: RawFd) -> io::Result<Editor> {
        let (w, h) = term_size::dimensions().expect("Failed to get terminal size.");
        let original = Editor {
            fd: fd,
            stdin: unsafe { FromRawFd::from_raw_fd(fd) },
            termios: Termios::from_fd(fd)?,
            poll: Poll::new()?,
            cx: 0,
            cy: 0,
            screen_rows: h as i32,
            screen_cols: w as i32,
        };

        let mut raw = original.termios.clone();

        raw.c_iflag &=
            !(termios::BRKINT | termios::ICRNL | termios::INPCK | termios::ISTRIP | termios::IXON);
        raw.c_oflag &= !(termios::OPOST);
        raw.c_cflag |= termios::CS8;
        raw.c_lflag &= !(termios::ECHO | termios::ICANON | termios::IEXTEN | termios::ISIG);
        raw.c_cc[termios::VMIN] = 0;
        // raw.c_cc[termios::VTIME] = 1;

        termios::tcsetattr(fd, termios::TCSAFLUSH, &raw)?;

        let stdin = EventedFd(&fd);

        original
            .poll
            .register(&stdin, Token(0), Ready::readable(), PollOpt::level())?;

        Ok(original)
    }
}

impl Drop for Editor {
    fn drop(&mut self) {
        termios::tcsetattr(self.fd, termios::TCSAFLUSH, &self.termios)
            .expect("Failed to revert terminal from raw mode.");
        io::stdout().flush().unwrap();
    }
}

// TODO: Restructure to not eat inputs if Esc is followed by a character
fn editor_read_key(stdin: &mut File, io_poll: &Poll) -> EditorKey {
    loop {
        match editor_queued_key(stdin, &io_poll, None) {
            Some(27) => {
                return match editor_queued_key(stdin, &io_poll, Some(Duration::new(0, 0))) {
                    Some(b'[') => {
                        match editor_queued_key(stdin, &io_poll, Some(Duration::new(0, 0))) {
                            Some(b'A') => EditorKey::ArrowUp,
                            Some(b'B') => EditorKey::ArrowDown,
                            Some(b'C') => EditorKey::ArrowRight,
                            Some(b'D') => EditorKey::ArrowLeft,
                            Some(b'H') => EditorKey::Home,
                            Some(b'F') => EditorKey::End,
                            Some(a) if a >= b'0' && a <= b'9' => {
                                match editor_queued_key(stdin, &io_poll, Some(Duration::new(0, 0)))
                                {
                                    Some(b'~') => match a {
                                        b'1' => EditorKey::Home,
                                        b'3' => EditorKey::Delete,
                                        b'4' => EditorKey::End,
                                        b'5' => EditorKey::PageUp,
                                        b'6' => EditorKey::PageDown,
                                        b'7' => EditorKey::Home,
                                        b'8' => EditorKey::End,
                                        _ => EditorKey::Key(a),
                                    },
                                    Some(b) => EditorKey::Key(b),
                                    None => EditorKey::Key(a),
                                }
                            }
                            Some(a) => EditorKey::Key(a),
                            None => EditorKey::Key(b'['),
                        }
                    }
                    Some(b'O') => {
                        match editor_queued_key(stdin, &io_poll, Some(Duration::new(0, 0))) {
                            Some(b'H') => EditorKey::Home,
                            Some(b'F') => EditorKey::End,
                            Some(a) => EditorKey::Key(a),
                            None => EditorKey::Key(b'O'),
                        }
                    }
                    _ => EditorKey::Key(27),
                }
            }
            Some(a) => return EditorKey::Key(a),
            None => continue,
        };
    }
}

fn editor_queued_key(stdin: &mut File, io_poll: &Poll, timeout: Option<Duration>) -> Option<u8> {
    let mut events = Events::with_capacity(1);

    io_poll
        .poll(&mut events, timeout)
        .expect("Stream closed unexpectedly.");

    for event in events.iter() {
        if event.readiness() == Ready::readable() {
            let mut buffer = [0; 1];
            stdin
                .read(&mut buffer)
                .expect("Failed to get character from stdin.");
            return Some(buffer[0]);
        }
    }

    None
}

fn truncate_to_editor_width(editor: &Editor, string: String) -> String {
    let graphemes = Graphemes::new(&string).take(editor.screen_cols as usize);
    graphemes.fold(String::new(), |mut acc, grapheme| {
        acc.push_str(grapheme);
        acc
    })
}

/*** output ***/

fn editor_draw_rows(editor: &Editor, buf: &mut String) {
    for y in 0..editor.screen_rows {
        if y == editor.screen_rows / 3 {
            let welcome =
                truncate_to_editor_width(editor, format!("Kilo editor -- version {}", VERSION));
            let padding = (editor.screen_cols - Graphemes::new(&welcome).count() as i32) / 2;
            if padding > 0 {
                buf.push_str("~");
            }
            for _i in 1..padding {
                buf.push_str(" ")
            }
            buf.push_str(&welcome);
        } else {
            buf.push_str("~");
        }

        buf.push_str("\x1b[K");
        if y < editor.screen_rows - 1 {
            buf.push_str("\r\n")
        }
    }
}

fn editor_refresh_screen(editor: &Editor) {
    let mut buf = String::new();

    buf.push_str("\x1b[?25l");
    buf.push_str("\x1b[H");

    editor_draw_rows(editor, &mut buf);

    buf.push_str(&format!("\x1b[{};{}H", editor.cy + 1, editor.cx + 1));
    buf.push_str("\x1b[?25h");

    io::stdout()
        .write_all(buf.as_bytes())
        .expect("Failed to print.");
    io::stdout().flush().unwrap();
}

/*** input ***/

fn editor_move_cursor(editor: &mut Editor, key: EditorKey) {
    match key {
        EditorKey::ArrowLeft => {
            if editor.cx != 0 {
                editor.cx -= 1
            }
        }
        EditorKey::ArrowRight => {
            if editor.cx != editor.screen_cols - 1 {
                editor.cx += 1
            }
        }
        EditorKey::ArrowUp => {
            if editor.cy != 0 {
                editor.cy -= 1
            }
        }
        EditorKey::ArrowDown => {
            if editor.cy != editor.screen_rows - 1 {
                editor.cy += 1
            }
        }
        _ => unreachable!(),
    }
}

fn editor_process_keypress(editor: &mut Editor) -> Option<EditorKey> {
    let c = editor_read_key(&mut editor.stdin, &editor.poll);
    match c {
        EditorKey::Key(17) => None,
        EditorKey::ArrowUp
        | EditorKey::ArrowLeft
        | EditorKey::ArrowDown
        | EditorKey::ArrowRight => {
            editor_move_cursor(editor, c);
            Some(c)
        }
        EditorKey::PageUp | EditorKey::PageDown => {
            for _ in 0..editor.screen_rows {
                match c {
                    EditorKey::PageUp => editor_move_cursor(editor, EditorKey::ArrowUp),
                    EditorKey::PageDown => editor_move_cursor(editor, EditorKey::ArrowDown),
                    _ => unreachable!(),
                }
            }
            Some(c)
        }
        EditorKey::Home => {
            editor.cx = 0;
            Some(c)
        }
        EditorKey::End => {
            editor.cx = editor.screen_cols - 1;
            Some(c)
        }
        _ => Some(c),
    }
}

/*** init ***/

fn main() {
    {
        let fd = io::stdin().as_raw_fd();
        let mut editor = Editor::from(fd).expect("Failed to enter raw mode.");

        loop {
            editor_refresh_screen(&editor);

            match editor_process_keypress(&mut editor) {
                Some(_) => continue,
                None => break,
            }
        }
    }

    print!("\x1b[2J");
    print!("\x1b[H");
    io::stdout().flush().unwrap();
}
