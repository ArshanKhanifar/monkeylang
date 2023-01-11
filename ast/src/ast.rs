use monkey_token::token::Token;

pub trait HasToken {
    fn token_literal(&self) -> &str;
}

#[derive(Debug, PartialEq)]
pub enum Expression {
    EMPTY, // TODO: Look wherever you used this and remove it
}

enum Node<'a> {
    Identifier(Token<'a>),
}

pub enum Statement<'a> {
    LetStatement {
        identifier: Token<'a>,
        expression: Expression,
    },
}

pub struct Program<'a> {
    pub statements: Vec<Statement<'a>>,
}
