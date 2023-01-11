use monkey_token::token::TokenType::{
    ASSIGN, ASTERISK, BANG, COMMA, EOF, EQ, GT, ILLEGAL, INT, LBRACE, LPAREN, LT, MINUS, NOT_EQ,
    PLUS, RBRACE, RPAREN, SEMICOLON, SLASH,
};
use monkey_token::token::{lookup_ident, Token, TokenType};

pub struct Lexer<'a> {
    input: &'a str,
    position: usize,
    read_position: usize,
    ch: Option<&'a str>,
}

fn is_letter(ch: char) -> bool {
    return 'a' <= ch && ch <= 'z' || 'A' <= ch && ch <= 'Z' || ch == '_';
}

fn is_digit(ch: char) -> bool {
    return '0' <= ch && ch <= '9';
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Lexer<'a> {
        let mut l = Lexer {
            input,
            position: 0,
            read_position: 0,
            ch: None,
        };
        l.read_char();
        l
    }

    fn peek(&self) -> Option<&str> {
        if self.read_position >= self.input.len() {
            return None;
        }
        return Some(&self.input[self.read_position..self.read_position + 1]);
    }

    fn skip_whitespace(&mut self) {
        while match self.ch.unwrap_or("_").chars().next().unwrap() {
            ' ' => true,
            '\t' => true,
            '\n' => true,
            '\r' => true,
            _ => false,
        } {
            self.read_char();
        }
    }

    fn read_char(&mut self) {
        if self.read_position >= self.input.len() {
            self.ch = None
        } else {
            self.ch = Some(&self.input[self.read_position..self.read_position + 1]);
        }
        self.position = self.read_position;
        self.read_position += 1;
    }

    pub fn read_identifier(&mut self) -> &'a str {
        self._read(is_letter)
    }

    pub fn read_number(&mut self) -> &'a str {
        self._read(is_digit)
    }

    fn _read(&mut self, meets_criteria: fn(char) -> bool) -> &'a str {
        let position = self.position;
        while let Some(ch) = self.ch {
            let ch = ch.chars().next().unwrap();
            if meets_criteria(ch) {
                self.read_char();
            } else {
                break;
            }
        }
        let literal = &self.input[position..self.position];
        literal
    }

    fn _single_char_token(&self, token_type: TokenType) -> Token<'a> {
        Token {
            token_type,
            literal: self.ch.unwrap(),
        }
    }

    pub fn next_token(&mut self) -> Token<'a> {
        self.skip_whitespace();
        let mut advanced = false;
        let tok = match self.ch {
            Some(ch) => match ch.chars().next().unwrap() {
                '=' => {
                    let next = self.peek();
                    if next == None || next.unwrap().chars().next().unwrap() != '=' {
                        Token {
                            token_type: ASSIGN,
                            literal: ch,
                        }
                    } else {
                        self.read_char();
                        Token {
                            token_type: EQ,
                            literal: "==",
                        }
                    }
                }
                '!' => {
                    let next = self.peek();
                    if next == None || next.unwrap().chars().next().unwrap() != '=' {
                        Token {
                            token_type: BANG,
                            literal: ch,
                        }
                    } else {
                        self.read_char();
                        Token {
                            token_type: NOT_EQ,
                            literal: "!=",
                        }
                    }
                }
                '+' => self._single_char_token(PLUS),
                '-' => self._single_char_token(MINUS),
                '*' => self._single_char_token(ASTERISK),
                '/' => self._single_char_token(SLASH),
                '<' => self._single_char_token(LT),
                '>' => self._single_char_token(GT),
                ',' => self._single_char_token(COMMA),
                ';' => self._single_char_token(SEMICOLON),
                '(' => self._single_char_token(LPAREN),
                ')' => self._single_char_token(RPAREN),
                '{' => self._single_char_token(LBRACE),
                '}' => self._single_char_token(RBRACE),
                _ => {
                    let c = ch.chars().next().unwrap();
                    advanced = true;
                    if is_letter(c) {
                        let literal = self.read_identifier();
                        Token {
                            token_type: lookup_ident(literal),
                            literal,
                        }
                    } else if is_digit(c) {
                        Token {
                            token_type: INT,
                            literal: self.read_number(),
                        }
                    } else {
                        Token {
                            token_type: ILLEGAL,
                            literal: ch,
                        }
                    }
                }
            },
            None => Token {
                token_type: EOF,
                literal: "",
            },
        };
        if !advanced {
            self.read_char();
        }
        tok
    }
}

#[cfg(test)]
mod tests {
    use monkey_token::token::TokenType::{ELSE, FALSE, FUNCTION, IDENT, IF, LET, RETURN, TRUE};

    use super::*;

    #[test]
    fn test_next_token_string_of_tokens() {
        let input = "=+(){},;";
        let tests = [
            (ASSIGN, "="),
            (PLUS, "+"),
            (LPAREN, "("),
            (RPAREN, ")"),
            (LBRACE, "{"),
            (RBRACE, "}"),
            (COMMA, ","),
            (SEMICOLON, ";"),
            (EOF, ""),
        ];
        let mut lexer = Lexer::new(input);

        for (token_type, literal) in tests {
            let next: Token = lexer.next_token();
            assert_eq!(next.token_type, token_type);
            assert_eq!(next.literal, literal);
        }
    }

    #[test]
    fn test_next_token_with_multiline_string_and_spaces() {
        let input = "let five = 52;
let ten = 10;

let add = fn(x, y) {
  x + y;
};

let result = add(five, ten);
";
        let tests = [
            (LET, "let"),
            (IDENT, "five"),
            (ASSIGN, "="),
            (INT, "52"),
            (SEMICOLON, ";"),
            (LET, "let"),
            (IDENT, "ten"),
            (ASSIGN, "="),
            (INT, "10"),
            (SEMICOLON, ";"),
            (LET, "let"),
            (IDENT, "add"),
            (ASSIGN, "="),
            (FUNCTION, "fn"),
            (LPAREN, "("),
            (IDENT, "x"),
            (COMMA, ","),
            (IDENT, "y"),
            (RPAREN, ")"),
            (LBRACE, "{"),
            (IDENT, "x"),
            (PLUS, "+"),
            (IDENT, "y"),
            (SEMICOLON, ";"),
            (RBRACE, "}"),
            (SEMICOLON, ";"),
            (LET, "let"),
            (IDENT, "result"),
            (ASSIGN, "="),
            (IDENT, "add"),
            (LPAREN, "("),
            (IDENT, "five"),
            (COMMA, ","),
            (IDENT, "ten"),
            (RPAREN, ")"),
            (SEMICOLON, ";"),
            (EOF, ""),
        ];

        let mut lexer = Lexer::new(input);

        for (token_type, literal) in tests {
            let next: Token = lexer.next_token();
            assert_eq!(next.token_type, token_type);
            assert_eq!(next.literal, literal);
        }
    }

    #[test]
    fn test_next_token_with_comments_and_operations() {
        let input = "let five = 5;
let ten=   10;

let add = fn(x, y) {
  x + y;
};

let result = add(five, ten);
!-/*5;
5 < 10 > 5;

if (5 < 10) {
    return true;
} else {
    return false;
}

10 == 10;
10 != 9;";
        let tests = [
            (LET, "let"),
            (IDENT, "five"),
            (ASSIGN, "="),
            (INT, "5"),
            (SEMICOLON, ";"),
            (LET, "let"),
            (IDENT, "ten"),
            (ASSIGN, "="),
            (INT, "10"),
            (SEMICOLON, ";"),
            (LET, "let"),
            (IDENT, "add"),
            (ASSIGN, "="),
            // fn(x, y)
            (FUNCTION, "fn"),
            (LPAREN, "("),
            (IDENT, "x"),
            (COMMA, ","),
            (IDENT, "y"),
            (RPAREN, ")"),
            (LBRACE, "{"),
            (IDENT, "x"),
            (PLUS, "+"),
            (IDENT, "y"),
            (SEMICOLON, ";"),
            (RBRACE, "}"),
            (SEMICOLON, ";"),
            // let result = add(five, ten);
            (LET, "let"),
            (IDENT, "result"),
            (ASSIGN, "="),
            (IDENT, "add"),
            (LPAREN, "("),
            (IDENT, "five"),
            (COMMA, ","),
            (IDENT, "ten"),
            (RPAREN, ")"),
            (SEMICOLON, ";"),
            // !-/*5;
            (BANG, "!"),
            (MINUS, "-"),
            (SLASH, "/"),
            (ASTERISK, "*"),
            (INT, "5"),
            (SEMICOLON, ";"),
            //5 < 10 > 5;
            (INT, "5"),
            (LT, "<"),
            (INT, "10"),
            (GT, ">"),
            (INT, "5"),
            (SEMICOLON, ";"),
            //if (5 < 10) {
            //    return true;
            //} else {
            //    return false;
            //}
            (IF, "if"),
            (LPAREN, "("),
            (INT, "5"),
            (LT, "<"),
            (INT, "10"),
            (RPAREN, ")"),
            (LBRACE, "{"),
            (RETURN, "return"),
            (TRUE, "true"),
            (SEMICOLON, ";"),
            (RBRACE, "}"),
            (ELSE, "else"),
            (LBRACE, "{"),
            (RETURN, "return"),
            (FALSE, "false"),
            (SEMICOLON, ";"),
            (RBRACE, "}"),
            // 10 == 10;
            (INT, "10"),
            (EQ, "=="),
            (INT, "10"),
            (SEMICOLON, ";"),
            // 10 != 9;
            (INT, "10"),
            (NOT_EQ, "!="),
            (INT, "9"),
            (SEMICOLON, ";"),
        ];

        let mut lexer = Lexer::new(input);

        for (token_type, literal) in tests {
            let next: Token = lexer.next_token();
            assert_eq!(next.token_type, token_type);
            assert_eq!(next.literal, literal);
        }
    }
}
