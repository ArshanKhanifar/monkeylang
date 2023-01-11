use monkey_token::token::Token;
use std::any::Any;

pub trait Node {
    fn token_literal(&self) -> &str;
}

pub trait Statement: Node {
    fn statement_node(&self);
}

pub trait Expression: Node {
    fn expression_node(&self);
}

pub struct Identifier<'a> {
    pub token: Token<'a>,
    pub value: &'a str,
}

impl<'a> Node for Identifier<'a> {
    fn token_literal(&self) -> &'a str {
        self.token.literal
    }
}

impl<'a> Expression for Identifier<'a> {
    fn expression_node(&self) {}
}

pub struct LetStatement<'a> {
    pub token: Token<'a>,
    pub name: Identifier<'a>,
    // TODO: make it not optional
    pub value: Option<Box<dyn Expression + 'a>>,
}

impl<'a> Node for LetStatement<'a> {
    fn token_literal(&self) -> &str {
        self.token.literal
    }
}

impl Statement for LetStatement<'_> {
    fn statement_node(&self) {}
}

pub struct Program<'a> {
    pub statements: Vec<Box<dyn Statement + 'a>>
}

impl<'a> Node for Program<'a> {
    fn token_literal(&self) -> &str {
        if !self.statements.is_empty() {
            self.statements[0].token_literal()
        } else {
            ""
        }
    }
}
