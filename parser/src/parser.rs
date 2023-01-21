use std::borrow::Borrow;
use std::collections::HashMap;
use std::hash::Hash;
use std::iter::Map;

use monkey_ast::ast::Expression::{
    Identifier, InfixExpression, IntegerLiteral, PrefixExpression, EMPTY,
};
use monkey_ast::ast::Statement::{ExpressionStatement, LetStatement, ReturnStatement};
use monkey_ast::ast::{Expression, Program, Statement};
use monkey_lexer::lexer::Lexer;
use monkey_token::token::TokenType::{
    ASTERISK, BANG, EQ, GT, IDENT, INT, LT, MINUS, NOT_EQ, PLUS, SEMICOLON, SLASH,
};
use monkey_token::token::{Token, TokenType};

use crate::parser::Precedence::{Equals, Lowest, Prefix};

#[derive(PartialOrd, PartialEq)]
enum Precedence {
    Lowest = 0,
    Equals,
    LessGreater,
    Sum,
    Product,
    Prefix,
    Call,
}

fn precedence_lookup(t: &TokenType) -> Precedence {
    match t {
        EQ => Equals,
        NOT_EQ => Equals,
        LT => Equals,
        GT => Equals,
        PLUS => Equals,
        MINUS => Equals,
        SLASH => Equals,
        ASTERISK => Equals,
        _ => Lowest,
    }
}

type ParseFn<'b, T> = fn(&mut T) -> Expression<'b>;

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

    fn peek_precedence(&self) -> Precedence {
        precedence_lookup(&self.peek_token.as_ref().unwrap().token_type)
    }

    fn curr_precedence(&self) -> Precedence {
        precedence_lookup(&self.curr_token.as_ref().unwrap().token_type)
    }

    fn parse_statement(&mut self) -> Option<Statement<'b>> {
        match self.curr_token.as_ref().unwrap().token_type {
            TokenType::LET => self.parse_let_statement(),
            TokenType::RETURN => self.parse_return_statement(),
            _ => self.parse_expression_statement(),
        }
    }

    fn parse_identifier(&mut self) -> Expression<'b> {
        Identifier(self.curr_token.take().unwrap())
    }

    fn parse_integer_literal(&mut self) -> Expression<'b> {
        let token = self.curr_token.take().unwrap();
        let value = token.literal.to_string().parse::<usize>().unwrap();
        IntegerLiteral { token, value }
    }

    fn parse_prefix(&mut self) -> Option<Expression<'b>> {
        match self.curr_token.as_ref().unwrap().token_type {
            IDENT => Some(self.parse_identifier()),
            INT => Some(self.parse_integer_literal()),
            BANG => Some(self.create_prefix_expression()),
            MINUS => Some(self.create_prefix_expression()),
            _ => None,
        }
    }

    fn get_infix_parse_fn(
        &mut self,
    ) -> Option<fn(&mut Parser<'a, 'b>, left: Expression<'b>) -> Expression<'b>> {
        match self.peek_token.as_ref().unwrap().token_type {
            PLUS => Some(Parser::parse_infix_expression),
            MINUS => Some(Parser::parse_infix_expression),
            SLASH => Some(Parser::parse_infix_expression),
            ASTERISK => Some(Parser::parse_infix_expression),
            EQ => Some(Parser::parse_infix_expression),
            NOT_EQ => Some(Parser::parse_infix_expression),
            LT => Some(Parser::parse_infix_expression),
            GT => Some(Parser::parse_infix_expression),
            _ => None,
        }
    }

    fn parse_expression(&mut self, precedence: Precedence) -> Option<Expression<'b>> {
        let prefix_result = self.parse_prefix();
        if prefix_result.is_none() {
            panic!("No prefix parser found!");
            // TODO: collect error here instead of panicking.
        }
        let mut left_exp = prefix_result.unwrap();
        while !self.peek_token_is(SEMICOLON) && precedence < self.peek_precedence() {
            let infix_fn = self.get_infix_parse_fn();
            if infix_fn.is_none() {
                return Some(left_exp);
            }
            self.next_token();
            left_exp = infix_fn.unwrap()(self, left_exp);
        }
        Some(left_exp)
    }

    fn parse_infix_expression(&mut self, left: Expression<'b>) -> Expression<'b> {
        let precedence = self.curr_precedence();
        let curr_token = self.curr_token.take().unwrap();
        self.next_token();
        InfixExpression {
            left: Box::new(left),
            operator: curr_token,
            right: Box::new(self.parse_expression(precedence).unwrap()),
        }
    }

    fn create_prefix_expression(&mut self) -> Expression<'b> {
        let curr_token = self.curr_token.take().unwrap();
        self.next_token();
        PrefixExpression {
            operator: curr_token,
            right: Box::new(self.parse_expression(Prefix).unwrap()),
        }
    }

    fn parse_expression_statement(&mut self) -> Option<Statement<'b>> {
        let expression = self.parse_expression(Precedence::Lowest);
        if expression.is_none() {
            return None;
        }
        let expression = expression.unwrap();
        if self.peek_token_is(SEMICOLON) {
            self.next_token();
        }
        return Some(ExpressionStatement { expression });
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

    fn parse_return_statement(&mut self) -> Option<Statement<'b>> {
        // TODO: we're skipping expression parsing
        while !self.curr_token_is(TokenType::SEMICOLON) {
            self.next_token();
        }
        Some(ReturnStatement {
            return_value: EMPTY,
        })
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

        Some(LetStatement {
            identifier: id_token,
            expression: EMPTY,
        })
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

macro_rules! setup_lexer_and_parser {
    ($lexer:ident, $parser:ident, $program: ident, $input:expr) => {
        let mut $lexer = Lexer::new($input);
        let mut $parser = Parser::new(&mut $lexer);
        let $program = $parser.parse_program();
        check_parser_errors(&$parser);
    };
}

#[cfg(test)]
mod tests {
    use std::any::Any;
    use std::mem::transmute;
    use std::ops::Deref;
    use std::os::macos::raw::stat;

    use monkey_ast::ast::Expression;
    use monkey_ast::ast::Expression::{IntegerLiteral, PrefixExpression};
    use monkey_token::token::TokenType::{IDENT, INT};

    use super::*;

    fn check_program_statements(program: &Program, num_statements: usize) {
        if program.statements.len() != num_statements {
            panic!(
                "Program does not contain {} statements, got: {}",
                num_statements,
                program.statements.len()
            );
        }
    }

    #[test]
    fn test_let_statements() {
        let input = "
let x = 5;
let y = 10;
let foobar = 838383;";
        setup_lexer_and_parser!(l, p, program, &input);
        check_program_statements(&program, 3);
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
            } else {
                panic!("expected LetStatement, got {:#?}", statement);
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
        );
    }

    #[test]
    fn test_return_statements() {
        let input = "
return 5;
return 10;
return 993322;";
        setup_lexer_and_parser!(l, p, program, &input);
        check_program_statements(&program, 3);

        for (i, statement) in program.statements.iter().enumerate() {
            if let ReturnStatement { return_value } = statement {
                assert_eq!(*return_value, EMPTY);
            } else {
                panic!("expected ReturnStatement, got {:#?}", statement);
            }
        }
    }

    #[test]
    fn test_identifier_expression() {
        let input = "foobar;";
        setup_lexer_and_parser!(l, p, program, &input);
        check_program_statements(&program, 1);
        let statement = program.statements.get(0).unwrap();
        let expression: &Expression = match statement {
            ExpressionStatement { expression } => expression,
            _ => panic!("expected ExpressionStatement, got {:#?}", statement),
        };
        let token = match expression {
            Identifier { 0: token } => token,
            _ => panic!("expected Identifier, got {:#?}", statement),
        };
        assert_eq!(token.token_type, IDENT);
        assert_eq!(token.literal, "foobar");
    }

    #[test]
    fn test_integer_literal_expression() {
        let input = "1234;";
        setup_lexer_and_parser!(l, p, program, &input);
        check_program_statements(&program, 1);
        let statement = program.statements.get(0).unwrap();
        let expression: &Expression = match statement {
            ExpressionStatement { expression } => expression,
            _ => panic!("expected ExpressionStatement, got {:#?}", statement),
        };
        let (token, value) = match expression {
            IntegerLiteral { token, value } => (token, value),
            _ => panic!("expected IntegerLiteral, got {:#?}", statement),
        };
        assert_eq!(token.token_type, INT);
        assert_eq!(token.literal, "1234");
        assert_eq!(*value, 1234);
    }

    #[test]
    fn test_parsing_prefix_operations() {
        let prefix_tests = [("!5", "!", "5"), ("-15", "-", "15")];
        for (i, (input, op, literal)) in prefix_tests.iter().enumerate() {
            setup_lexer_and_parser!(l, p, program, &input);
            check_program_statements(&program, 1);
            let statement = program.statements.get(0).unwrap();
            let expression: &Expression = match statement {
                ExpressionStatement { expression } => expression,
                _ => panic!("expected ExpressionStatement, got {:#?}", statement),
            };
            let (operator, right) = match expression {
                PrefixExpression { operator, right } => (operator, right),
                _ => panic!("expected IntegerLiteral, got {:#?}", statement),
            };
            assert_eq!(*op, operator.literal);
            let (token, value) = match right.as_ref() {
                IntegerLiteral { token, value } => (token, value),
                _ => panic!("expected IntegerLiteral, got {:#?}", statement),
            };
            assert_eq!(token.literal, *literal);
        }
    }

    #[test]
    fn test_parsing_infix_operations() {
        let infix_tests = [
            ("5 + 5;", 5, "+", 5),
            ("5 - 5;", 5, "-", 5),
            ("5 * 5;", 5, "*", 5),
            ("5 / 5;", 5, "/", 5),
            ("5 > 5;", 5, ">", 5),
            ("5 < 5;", 5, "<", 5),
            ("5 == 5;", 5, "==", 5),
            ("5 != 5;", 5, "!=", 5),
        ];

        for (input, l_val, op, r_val) in infix_tests {
            setup_lexer_and_parser!(l, p, program, &input);
            check_program_statements(&program, 1);
        }
    }
}
