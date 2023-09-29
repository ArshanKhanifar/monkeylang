use std::io;
use std::io::{BufRead, BufReader};

use monkey_lexer::lexer::Lexer;
use monkey_parser::parser::Parser;

const PROMPT: &str = ">> ";

const MONKEY_FACE: &str = "
   .--.  .-\"     \"-.  .--.
  / .. \\/  .-. .-.  \\/ .. \\
 | |  '|  /   Y   \\  |'  | |
 | \\   \\  \\ 0 | 0 /  /   / |
  \\ '- ,\\.-\"\"\"\"\"\"\"-./, -' /
   ''-' /_   ^ ^   _\\ '-''
       |  \\._   _./  |
       \\   \\ '~' /   /
        '._ '-=-' _.'
           '-----'
";

fn print_parse_errors<W: io::Write>(writer: &mut W, errors: &Vec<String>) {
    write!(writer, "{}", MONKEY_FACE).unwrap();
    write!(writer, "Whoops! We ran into some monkey business here!\n").unwrap();
    write!(writer, "\tparser errors:\n").unwrap();
    for msg in errors {
        write!(writer, "{}", msg).unwrap();
    }
}

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
        let mut p = Parser::new(&mut l);
        let program = p.parse_program();
        if p.errors().len() != 0 {
            print_parse_errors(&mut writer, p.errors());
        }
        write!(writer, "{}\n", program.to_string());
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        // just passes! 🎉
    }
}
