use monkey_ast::ast::Expression::EMPTY;
use monkey_ast::ast::Statement::LetStatement;
use monkey_ast::ast::{Program, Statement};
use monkey_lexer::lexer::Lexer;
use monkey_token::token::{Token, TokenType};

struct Parser<'a, 'b: 'a> {
    l: &'a mut Lexer<'b>,
    errors: Vec<String>,
    curr_token: Option<Token<'b>>,
    peek_token: Option<Token<'b>>,
}

impl<'a, 'b: 'a> Parser<'a, 'b> {
    fn new(l: &'a mut Lexer<'b>) -> Parser<'a, 'b> {
        let curr_token = Some(l.next_token());
        let peek_token = Some(l.next_token());
        Parser {
            l,
            errors: Vec::new(),
            curr_token,
            peek_token,
        }
    }

    fn next_token(&mut self) {
        self.curr_token = self.peek_token.take();
        self.peek_token = Some(self.l.next_token());
    }

    fn parse_statement(&mut self) -> Option<Statement<'b>> {
        match self.curr_token.as_ref().unwrap().token_type {
            TokenType::LET => self.parse_let_statement(),
            _ => None,
        }
    }

    pub fn errors(&self) -> &Vec<String> {
        &self.errors
    }

    fn peek_error(&mut self, t: TokenType) {
        let msg = format!(
            "expected next token to be {}, got {} instead",
            t.to_str(),
            self.peek_token.as_ref().unwrap().token_type.to_str()
        );
        self.errors.push(msg);
    }

    fn parse_let_statement(&mut self) -> Option<Statement<'b>> {
        if !self.expect_peek(TokenType::IDENT) {
            return None;
        }
        let id_token = self.curr_token.take().unwrap();
        if !self.expect_peek(TokenType::ASSIGN) {
            return None;
        }
        // TODO: we're skipping expression parsing
        while !self.curr_token_is(TokenType::SEMICOLON) {
            self.next_token();
        }

        return Some(LetStatement {
            identifier: id_token,
            expression: EMPTY,
        });
    }

    fn curr_token_is(&self, t: TokenType) -> bool {
        self.curr_token.as_ref().unwrap().token_type == t
    }

    fn peek_token_is(&self, t: TokenType) -> bool {
        self.peek_token.as_ref().unwrap().token_type == t
    }

    fn expect_peek(&mut self, t: TokenType) -> bool {
        if self.peek_token_is(t) {
            self.next_token();
            true
        } else {
            false
        }
    }

    fn parse_program(&mut self) -> Program<'b> {
        let mut program = Program {
            statements: Vec::new(),
        };
        while !self.curr_token_is(TokenType::EOF) {
            if let Some(statement) = self.parse_statement() {
                program.statements.push(statement);
            }
            self.next_token();
        }
        program
    }
}

#[cfg(test)]
mod tests {
    use std::any::Any;
    use std::mem::transmute;
    use std::os::macos::raw::stat;

    use super::*;

    #[test]
    fn test_let_statements() {
        let input = "
let x = 5;
let y = 10;
let foobar = 838383;";
        let mut l = Lexer::new(input);
        let mut p = Parser::new(&mut l);
        let program = p.parse_program();
        check_parser_errors(&p);

        if program.statements.len() != 3 {
            panic!(
                "Program does not contain 3 statements, got: {}",
                program.statements.len()
            );
        }
        let tests = ["x", "y", "foobar"];
        for (i, expected_identifier) in tests.iter().enumerate() {
            let mut statement = program.statements.get(i);
            assert!(statement.is_some());
            let statement = statement.unwrap();
            if let LetStatement {
                identifier,
                expression,
            } = statement
            {
                assert_eq!(identifier.literal, *expected_identifier);
                assert_eq!(*expression, EMPTY);
            }
        }
    }

    fn check_parser_errors(p: &Parser) {
        let errors = p.errors();
        assert_eq!(
            errors.len(),
            0,
            "had parsing errors: {}",
            errors.get(0).unwrap()
        )
    }
}
