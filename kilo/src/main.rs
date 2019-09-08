extern crate termios;

use std::io;
use std::io::Read;
use std::os::unix::io::AsRawFd;
use std::os::unix::io::RawFd;

use termios::Termios;

struct RawMode {
    fd: RawFd,
    termios: Termios
}

impl RawMode {
    fn from(fd: RawFd) -> io::Result<RawMode> {
        let original = RawMode {
            fd: fd,
            termios: Termios::from_fd(fd)?
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

impl Drop for RawMode {
    fn drop(&mut self) {
        termios::tcsetattr(self.fd, termios::TCSAFLUSH, &self.termios)
            .expect("Failed to revert terminal from raw mode.");
    }
}

fn main() {
    let fd = io::stdin().as_raw_fd();
    let _raw_mode = RawMode::from(fd)
        .expect("Failed to enter raw mode.");

    loop {
        let mut buffer = [0];
        match io::stdin().read(&mut buffer) {
            Err(_) => panic!("Failed to read character from STDIN."),
            Ok(_) => {
                let c = buffer[0] as char;
                match c{
                    'q' => break,
                    _ => print!("{:?}\r\n", c)
                }
            }
        }
    }
}
