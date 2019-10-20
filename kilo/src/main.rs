extern crate termios;

use std::io;
use std::io::Read;
use std::io::Write;
use std::os::unix::io::AsRawFd;
use std::os::unix::io::RawFd;

use termios::Termios;
use unic_segment::Graphemes;

const VERSION: &'static str = env!("CARGO_PKG_VERSION");

/*** data ***/

struct Editor {
    fd: RawFd,
    termios: Termios,
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
    Key(u8)
}

/*** terminal ***/

impl Editor {
    fn from(fd: RawFd) -> io::Result<Editor> {
        let (w, h) = term_size::dimensions().expect("Failed to get terminal size.");
        let original = Editor {
            fd: fd,
            termios: Termios::from_fd(fd)?,
            cx: 0,
            cy: 0,
            screen_rows: h as i32,
            screen_cols: w as i32,
        };

        let mut raw = original.termios.clone();

        raw.c_iflag &= !(termios::BRKINT |
                         termios::ICRNL |
                         termios::INPCK |
                         termios::ISTRIP |
                         termios::IXON);
        raw.c_oflag &= !(termios::OPOST);
        raw.c_cflag |=   termios::CS8;
        raw.c_lflag &= !(termios::ECHO |
                         termios::ICANON |
                         termios::IEXTEN |
                         termios::ISIG);
        raw.c_cc[termios::VMIN] = 0;
        raw.c_cc[termios::VTIME] = 1;

        termios::tcsetattr(fd, termios::TCSAFLUSH, &raw)?;

        Ok(original)
    }
}

impl Drop for Editor {
    fn drop(&mut self) {
        termios::tcsetattr(self.fd, termios::TCSAFLUSH, &self.termios)
            .expect("Failed to revert terminal from raw mode.");
    }
}

fn editor_read_key() -> Result<EditorKey, &'static str> {
    let mut buffer = [0];
    match io::stdin().read(&mut buffer) {
        Err(_) => Err("Failed to read character from STDIN."),
        Ok(_) => {
            match buffer[0] {
                // 27 => {
                //     match editor_read_key()? {
                //         EditorKey::Key('[') =>
                //     }
                // }
                _ => Ok(EditorKey::Key(buffer[0]))
            }
        }
    }
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

    io::stdout().write_all(buf.as_bytes()).expect("Failed to print.");
    io::stdout().flush().unwrap();
}

/*** input ***/

fn editor_move_cursor(editor: &mut Editor, key: EditorKey) -> Result<(), &'static str> {
    match key {
        EditorKey::ArrowLeft => if editor.cx != 0 {editor.cx -= 1},
        EditorKey::ArrowRight => if editor.cx != editor.screen_cols - 1 {editor.cx += 1}
        EditorKey::ArrowUp => if editor.cy != 0 {editor.cy -= 1}
        EditorKey::ArrowDown => if editor.cy != editor.screen_rows - 1 {editor.cy += 1}
        _ => return Err("This should never happen.")
    };
    Ok(())
}

fn editor_process_keypress(editor: &mut Editor) -> Result<Option<EditorKey>, &'static str> {
    let c = editor_read_key()?;
    match c {
        EditorKey::Key(17) => Ok(None),
        EditorKey::ArrowUp | EditorKey::ArrowLeft | EditorKey::ArrowDown | EditorKey::ArrowRight => {
            editor_move_cursor(editor, c)?;
            Ok(Some(c))
        }
        _ => Ok(Some(c)),
    }
}

/*** init ***/

fn main() {
    let mut errno = "Unknown Error.";
    let mut safe_exit = false;

    {
        let fd = io::stdin().as_raw_fd();
        let mut editor = Editor::from(fd).expect("Failed to enter raw mode.");

        loop {
            editor_refresh_screen(&editor);
            print!("{}", editor.cx);
            match editor_process_keypress(&mut editor) {
                Err(e) => {
                    errno = e;
                    break;
                }
                Ok(Some(_)) => continue,
                Ok(None) => {
                    safe_exit = true;
                    break;
                }
            }
        }
    }

    print!("\x1b[2J");
    print!("\x1b[H");
    io::stdout().flush().unwrap();

    if safe_exit {
        std::process::exit(0);
    }
    panic!(errno);
}
