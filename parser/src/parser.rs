use std::borrow::Borrow;
use std::collections::HashMap;
use std::hash::Hash;
use std::iter::Map;

use monkey_ast::ast::Expression::{
    BooleanLiteral, CallExpression, Identifier, IfExpression, InfixExpression, IntegerLiteral,
    PrefixExpression, EMPTY,
};
use monkey_ast::ast::Statement::{ExpressionStatement, LetStatement, ReturnStatement};
use monkey_ast::ast::{BlockStatement, Expression, Program, Statement};
use monkey_lexer::lexer::Lexer;
use monkey_token::token::TokenType::{
    ASTERISK, BANG, COMMA, ELSE, EOF, EQ, FALSE, GT, IDENT, IF, ILLEGAL, INT, LBRACE, LPAREN, LT,
    MINUS, NOT_EQ, PLUS, RBRACE, RPAREN, SEMICOLON, SLASH, TRUE,
};
use monkey_token::token::{Token, TokenType};

use crate::parser::Precedence::{Call, Equals, LessGreater, Lowest, Prefix, Product, Sum};

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
        LT => LessGreater,
        GT => LessGreater,
        PLUS => Sum,
        MINUS => Sum,
        SLASH => Product,
        ASTERISK => Product,
        LPAREN => Call,
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
        Identifier(self.curr_token.unwrap().clone())
    }

    fn parse_boolean(&mut self) -> Expression<'b> {
        BooleanLiteral(self.curr_token.unwrap().clone().literal == "true")
    }

    fn parse_integer_literal(&mut self) -> Expression<'b> {
        let token = self.curr_token.unwrap().clone();
        let value = token.literal.to_string().parse::<usize>().unwrap();
        IntegerLiteral { token, value }
    }

    fn parse_prefix(&mut self) -> Option<Expression<'b>> {
        match self.curr_token.as_ref().unwrap().token_type {
            IDENT => Some(self.parse_identifier()),
            INT => Some(self.parse_integer_literal()),
            BANG => Some(self.create_prefix_expression()),
            MINUS => Some(self.create_prefix_expression()),
            TRUE => Some(self.parse_boolean()),
            FALSE => Some(self.parse_boolean()),
            LPAREN => self.parse_grouped_expression(),
            IF => self.parse_if_expression(),
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
            LPAREN => Some(Parser::parse_call_expression),
            _ => None,
        }
    }

    fn parse_expression(&mut self, precedence: Precedence) -> Option<Expression<'b>> {
        let prefix_result = self.parse_prefix();
        if prefix_result.is_none() {
            let msg = format!(
                "No prefix parse function for {} found",
                self.curr_token
                    .as_ref()
                    .unwrap_or(&Token {
                        literal: "DIDNT FIND",
                        token_type: ILLEGAL
                    })
                    .literal
                    .to_string()
            );
            self.errors.push(msg);
            return None;
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

    fn parse_call_expression(&mut self, e: Expression<'b>) -> Expression<'b> {
        CallExpression {
            function: Box::new(e),
            arguments: self.parse_call_arguments(),
        }
    }

    fn parse_call_arguments(&mut self) -> Vec<Expression<'b>> {
        let mut args = Vec::new();
        if self.peek_token_is(RPAREN) {
            self.next_token();
            return args;
        }
        self.next_token();
        args.push(self.parse_expression(Lowest).unwrap());

        while self.peek_token_is(COMMA) {
            self.next_token();
            self.next_token();
            args.push(self.parse_expression(Lowest).unwrap());
        }

        if !self.expect_peek(RPAREN) {
            panic!("expected RPAREN");
        }

        args
    }

    fn parse_infix_expression(&mut self, left: Expression<'b>) -> Expression<'b> {
        let precedence = self.curr_precedence();
        let curr_token = self.curr_token.unwrap().clone();
        self.next_token();
        InfixExpression {
            left: Box::new(left),
            operator: curr_token,
            right: Box::new(self.parse_expression(precedence).unwrap()),
        }
    }

    fn create_prefix_expression(&mut self) -> Expression<'b> {
        let curr_token = self.curr_token.unwrap().clone();
        self.next_token();
        PrefixExpression {
            operator: curr_token,
            right: Box::new(self.parse_expression(Prefix).unwrap()),
        }
    }

    fn parse_grouped_expression(&mut self) -> Option<Expression<'b>> {
        self.next_token();
        let exp = self.parse_expression(Lowest);
        if !self.expect_peek(RPAREN) {
            return None;
        }
        exp
    }

    fn parse_if_expression(&mut self) -> Option<Expression<'b>> {
        if !self.expect_peek(LPAREN) {
            return None;
        }
        self.next_token();
        let condition = self.parse_expression(Lowest).unwrap();
        if !self.expect_peek(RPAREN) {
            return None;
        }
        if !self.expect_peek(LBRACE) {
            return None;
        }
        let consequence = self.parse_block_statement();
        let mut alternative = None;

        if self.peek_token_is(ELSE) {
            self.next_token();
            if !self.expect_peek(LBRACE) {
                return None;
            }
            alternative = Some(self.parse_block_statement());
        }

        Some(IfExpression {
            condition: Box::new(condition),
            consequence,
            alternative,
        })
    }

    fn parse_block_statement(&mut self) -> BlockStatement<'b> {
        self.next_token();
        let mut statements = Vec::new();
        while !self.curr_token_is(RBRACE) && !self.curr_token_is(EOF) {
            let stmnt = self.parse_statement().unwrap();
            statements.push(stmnt);
            self.next_token();
        }
        BlockStatement { statements }
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
        self.next_token();
        let expression = self.parse_expression(Lowest);
        if expression.is_none() {
            return Some(ReturnStatement { expression: EMPTY });
        }
        if self.peek_token_is(SEMICOLON) {
            self.next_token();
        }
        Some(ReturnStatement {
            expression: expression.unwrap(),
        })
    }

    fn parse_let_statement(&mut self) -> Option<Statement<'b>> {
        if !self.expect_peek(TokenType::IDENT) {
            return None;
        }
        let id_token = self.curr_token.unwrap().clone();
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
            self.peek_error(t);
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

    fn check_program_statements_length(program: &Program, num_statements: usize) {
        if program.statements.len() != num_statements {
            panic!(
                "Program does not contain {} statements, got: {}",
                num_statements,
                program.statements.len()
            );
        }
    }

    fn expression_from_statement<'a>(s: &'a Statement<'a>) -> &'a Expression<'a> {
        match s {
            ExpressionStatement { expression } => &expression,
            _ => panic!("expected ExpressionStatement, got {:#?}", s),
        }
    }

    fn extract_program_expression<'a>(
        program: &'a Program<'a>,
        index: usize,
    ) -> &'a Expression<'a> {
        let statement = program.statements.get(index).unwrap();
        expression_from_statement(statement)
        //match statement { ExpressionStatement { expression } => expression,
        //    _ => panic!("expected ExpressionStatement, got {:#?}", statement),
        //}
    }

    fn check_and_extract_program_expression<'a>(program: &'a Program<'a>) -> &'a Expression<'a> {
        check_program_statements_length(&program, 1);
        return extract_program_expression(&program, 0);
    }

    #[test]
    fn test_let_statements() {
        let input = "
let x = 5;
let y = 10;
let foobar = 838383;";
        setup_lexer_and_parser!(l, p, program, &input);
        check_program_statements_length(&program, 3);
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
            "had parsing errors:\n\t{}",
            errors.join("\n\t")
        );
    }

    #[test]
    fn test_return_statements() {
        let input = "
return 5;
return 10;
return 993322;";
        setup_lexer_and_parser!(l, p, program, &input);
        check_program_statements_length(&program, 3);

        let values = [5, 10, 993322];
        for (i, v) in values.iter().enumerate() {
            let statement = program.statements.get(i).unwrap();
            if let ReturnStatement { expression } = statement {
                check_integer_literal(expression, *v);
            } else {
                panic!("expected ReturnStatement, got {:#?}", statement);
            }
        }
    }

    #[test]
    fn test_identifier_expression() {
        let input = "foobar;";
        setup_lexer_and_parser!(l, p, program, &input);
        let expression = check_and_extract_program_expression(&program);
        check_identifier(expression, "foobar");
    }

    #[test]
    fn test_integer_literal_expression() {
        let input = "1234;";
        setup_lexer_and_parser!(l, p, program, &input);
        let expression = check_and_extract_program_expression(&program);
        check_integer_literal(expression, 1234);
    }

    fn test_prefix_expression<T>(e: &Expression, op: &str, val: T, tester: fn(&Expression, T)) {
        let (operator, right) = match e {
            PrefixExpression { operator, right } => (operator, right),
            _ => panic!("expected PrefixExpression, got {:#?}", e),
        };
        tester(right.as_ref(), val);
        assert_eq!(op, operator.literal);
    }

    #[test]
    fn test_parsing_prefix_operations() {
        let prefix_tests = [("!5", "!", "5"), ("-15", "-", "15")];
        for (i, (input, op, literal)) in prefix_tests.iter().enumerate() {
            setup_lexer_and_parser!(l, p, program, &input);
            let expression = check_and_extract_program_expression(&program);
            test_prefix_expression::<usize>(
                expression,
                *op,
                (*literal).parse::<usize>().unwrap(),
                check_integer_literal,
            );
        }
    }

    fn check_infix_expression<T, U>(
        e: &Expression,
        l_val: T,
        l_tester: fn(&Expression, T),
        op: &str,
        r_val: U,
        r_tester: fn(&Expression, U),
    ) {
        let (left, operator, right) = match e {
            InfixExpression {
                left,
                operator,
                right,
            } => (left, operator, right),
            _ => panic!("expected InfixExpression, got {:#?}", e),
        };
        l_tester(left.as_ref(), l_val);
        assert_eq!(op, operator.literal);
        r_tester(right.as_ref(), r_val);
    }

    fn check_identifier(e: &Expression, value: &str) {
        let Token {
            literal,
            token_type,
        } = match e {
            Identifier(t) => t,
            _ => panic!("expected InfixExpression, got {:#?}", e),
        };
        assert_eq!(*token_type, IDENT);
        assert_eq!(*literal, value);
    }

    fn test_boolean_literal(e: &Expression, value: bool) {
        let val = match e {
            BooleanLiteral(val) => val,
            _ => panic!("expected BooleanLiteral, got {:#?}", e),
        };
        assert_eq!(*val, value);
    }

    fn check_integer_literal(e: &Expression, value: usize) {
        let (
            Token {
                literal,
                token_type,
            },
            v,
        ) = match e {
            IntegerLiteral { token, value } => (token, value),
            _ => panic!("expected IntegerLiteral, got {:#?}", e),
        };
        assert_eq!(*v, value);
        assert_eq!(*token_type, INT);
        assert_eq!(*literal, format!("{}", value));
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

        let bool_tests = [
            ("true == true", true, "==", true),
            ("true != false", true, "!=", false),
            ("false == false", false, "==", false),
        ];

        for (input, l_val, op, r_val) in infix_tests {
            setup_lexer_and_parser!(l, p, program, &input);
            let expression = check_and_extract_program_expression(&program);
            check_infix_expression::<usize, usize>(
                expression,
                l_val,
                check_integer_literal,
                op,
                r_val,
                check_integer_literal,
            );
        }

        for (input, l_val, op, r_val) in bool_tests {
            setup_lexer_and_parser!(l, p, program, &input);
            let expression = check_and_extract_program_expression(&program);
            check_infix_expression::<bool, bool>(
                expression,
                l_val,
                test_boolean_literal,
                op,
                r_val,
                test_boolean_literal,
            );
        }
    }

    #[test]
    fn test_operator_precedence_parsing() {
        let precedence_tests = [
            ("-a * b", "((-a) * b)"),
            ("!-a", "(!(-a))"),
            ("a + b + c", "((a + b) + c)"),
            ("a + b - c", "((a + b) - c)"),
            ("a * b * c", "((a * b) * c)"),
            ("a * b / c", "((a * b) / c)"),
            ("a + b / c", "(a + (b / c))"),
            ("a + b * c + d / e - f", "(((a + (b * c)) + (d / e)) - f)"),
            ("3 + 4; -5 * 5", "(3 + 4)((-5) * 5)"),
            ("5 > 4 == 3 < 4", "((5 > 4) == (3 < 4))"),
            ("5 < 4 != 3 > 4", "((5 < 4) != (3 > 4))"),
            (
                "3 + 4 * 5 == 3 * 1 + 4 * 5",
                "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))",
            ),
            // booleans
            ("true", "true"),
            ("false", "false"),
            ("3 > 5 == false", "((3 > 5) == false)"),
            ("3 < 5 == true", "((3 < 5) == true)"),
            // brackets
            ("1 + (2 + 3) + 4", "((1 + (2 + 3)) + 4)"),
            ("(5 + 5) * 2", "((5 + 5) * 2)"),
            ("2 / (5 + 5)", "(2 / (5 + 5))"),
            ("-(5 + 5)", "(-(5 + 5))"),
            ("!(true == true)", "(!(true == true))"),
            // functions
            ("a + add(b * c) + d", "((a + add((b * c))) + d)"),
            (
                "add(a, b, 1, 2 * 3, 4 + 5, add(6, 7 * 8))",
                "add(a, b, 1, (2 * 3), (4 + 5), add(6, (7 * 8)))",
            ),
            (
                "add(a + b + c * d / f + g)",
                "add((((a + b) + ((c * d) / f)) + g))",
            ),
        ];

        for (input, expected) in precedence_tests {
            setup_lexer_and_parser!(l, p, program, &input);
            let actual = program.to_string();
            assert_eq!(actual, expected);
        }
    }

    fn check_if_else_expression(
        e: &Expression,
        condition_tester: fn(e: &Expression),
        consequence_tester: fn(e: &BlockStatement),
        alternative_tester: Option<fn(e: &BlockStatement)>,
    ) {
        let (condition, consequence, alternative) = match e {
            IfExpression {
                condition,
                consequence,
                alternative,
            } => (condition.as_ref(), consequence, alternative),
            _ => panic!("expected IfExpression, got {:#?}", e),
        };
        condition_tester(condition);
        consequence_tester(consequence);
        if let Some(alternative_tester) = alternative_tester {
            if let Some(alternative) = alternative {
                alternative_tester(alternative);
            }
        }
    }

    #[test]
    fn test_if_expression() {
        let input = "if (x < y) {abc}";
        setup_lexer_and_parser!(l, p, program, &input);
        let expression = check_and_extract_program_expression(&program);
        check_if_else_expression(
            expression,
            |condition| {
                check_infix_expression::<&str, &str>(
                    condition,
                    "x",
                    |e, l| check_identifier(e, l),
                    "<",
                    "y",
                    |e, r| check_identifier(e, r),
                )
            },
            |BlockStatement { statements }| {
                assert_eq!(statements.len(), 1);
                let s = statements.get(0).unwrap();
                let e = expression_from_statement(s);
                check_identifier(e, "abc");
            },
            None,
        );
    }

    #[test]
    fn test_if_else_expression() {
        let input = "if (x < y) {arshan} else {arshia;bob} ";
        setup_lexer_and_parser!(l, p, program, &input);
        let expression = check_and_extract_program_expression(&program);
        check_if_else_expression(
            expression,
            |condition| {
                check_infix_expression::<&str, &str>(
                    condition,
                    "x",
                    |e, l| check_identifier(e, l),
                    "<",
                    "y",
                    |e, r| check_identifier(e, r),
                )
            },
            |BlockStatement { statements }| {
                assert_eq!(statements.len(), 1);
                let s = statements.get(0).unwrap();
                let e = expression_from_statement(s);
                check_identifier(e, "arshan");
            },
            Some(|BlockStatement { statements }| {
                assert_eq!(statements.len(), 2);
                let s = statements.get(0).unwrap();
                let e = expression_from_statement(s);
                check_identifier(e, "arshia");
                let s = statements.get(1).unwrap();
                let e = expression_from_statement(s);
                check_identifier(e, "bob");
            }),
        );
    }

    #[test]
    fn test_call_expression_parsing() {
        let input = "add(1, 2 * 3, 4 + 5);";
        setup_lexer_and_parser!(l, p, program, &input);
        let expression = check_and_extract_program_expression(&program);
        let (function, arguments) = match expression {
            CallExpression {
                function,
                arguments,
            } => (function, arguments),
            _ => panic!("expected CallExpression, got {:#?}", expression),
        };
        check_identifier(function.as_ref(), "add");
        assert_eq!(arguments.len(), 3);
        let (a, b, c) = (
            arguments.get(0).unwrap(),
            arguments.get(1).unwrap(),
            arguments.get(2).unwrap(),
        );
        check_integer_literal(a, 1);
        check_infix_expression::<usize, usize>(
            b,
            2,
            check_integer_literal,
            "*",
            3,
            check_integer_literal,
        );
        check_infix_expression::<usize, usize>(
            c,
            4,
            check_integer_literal,
            "+",
            5,
            check_integer_literal,
        );
    }
}
