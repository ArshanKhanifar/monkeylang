use monkey_lexer::lexer::Lexer;
use monkey_token::token::EOF;
use std::io;
use std::io::{BufRead, BufReader};

const PROMPT: &str = ">> ";

pub fn start<R: io::Read, W: io::Write>(reader: R, mut writer: W) {
    write!(writer, "{}", PROMPT).unwrap();
    let reader = BufReader::new(reader);
    let mut lines = reader.lines();
    loop {
        println!("waiting for next line");
        let scanned = match lines.next() {
            Some(Ok(line)) => line,
            _ => return,
        };
        println!("scanned line: {scanned}");

        let mut l = Lexer::new(scanned.as_str());

        loop {
            let tok = l.next_token();
            if tok.token_type == EOF {
                break;
            }
            write!(writer, "{:?}\n", tok).unwrap();
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        let result = add(2, 2);
        assert_eq!(result, 4);
    }
}
