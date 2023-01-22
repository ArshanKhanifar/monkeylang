use monkey_token::token::Token;
use std::fmt::format;

pub trait HasToken {
    fn token_literal(&self) -> &str;
}

#[derive(Debug)]
pub enum Expression<'a> {
    Identifier(Token<'a>),
    BooleanLiteral(bool),
    IntegerLiteral {
        token: Token<'a>,
        value: usize,
    },
    PrefixExpression {
        operator: Token<'a>,
        right: Box<Expression<'a>>,
    },
    InfixExpression {
        operator: Token<'a>,
        left: Box<Expression<'a>>,
        right: Box<Expression<'a>>,
    },
    IfExpression {
        condition: Box<Expression<'a>>,
        consequence: BlockStatement<'a>,
        alternative: Option<BlockStatement<'a>>,
    },
    EMPTY, // TODO: Look wherever you used this and remove it
}

impl<'a> ToString for Expression<'a> {
    fn to_string(&self) -> String {
        match self {
            Expression::Identifier(token) => token.literal.to_string(),
            Expression::BooleanLiteral(val) => val.to_string(),
            Expression::IntegerLiteral { token, value } => token.literal.to_string(),
            Expression::PrefixExpression { operator, right } => {
                format!("({}{})", operator.literal, right.to_string())
            }
            Expression::IfExpression {
                condition,
                consequence,
                alternative,
            } => {
                let mut s = format!(
                    "if {} {{{}}}",
                    condition.to_string(),
                    consequence.to_string(),
                );
                if let Some(alternative) = alternative {
                    s.push_str(&*format!(" else {{{}}}", alternative.to_string()));
                }
                s
            }
            Expression::InfixExpression {
                operator,
                left,
                right,
            } => {
                format!(
                    "({} {} {})",
                    left.to_string(),
                    operator.literal,
                    right.to_string()
                )
            }
            _ => "".to_string(),
        }
    }
}

#[derive(Debug)]
pub enum Statement<'a> {
    LetStatement {
        identifier: Token<'a>,
        expression: Expression<'a>,
    },
    ReturnStatement {
        expression: Expression<'a>,
    },
    ExpressionStatement {
        expression: Expression<'a>,
    },
}

#[derive(Debug)]
pub struct BlockStatement<'a> {
    pub statements: Vec<Statement<'a>>,
}

impl<'a> ToString for BlockStatement<'a> {
    fn to_string(&self) -> String {
        self.statements
            .iter()
            .map(|x| x.to_string())
            .collect::<Vec<String>>()
            .join("")
    }
}

impl<'a> ToString for Statement<'a> {
    fn to_string(&self) -> String {
        match self {
            Statement::LetStatement {
                identifier,
                expression,
            } => format!("let {} = {};", identifier.literal, expression.to_string()).to_string(),
            Statement::ReturnStatement { expression: return_value } => {
                format!("return {};", return_value.to_string()).to_string()
            }
            Statement::ExpressionStatement { expression } => {
                format!("{}", expression.to_string()).to_string()
            }
            _ => {
                panic!("Invalid statement")
            }
        }
    }
}

pub struct Program<'a> {
    pub statements: Vec<Statement<'a>>,
}

impl<'a> ToString for Program<'a> {
    fn to_string(&self) -> String {
        let mut out = String::new();

        for s in &self.statements {
            out.push_str(&s.to_string());
        }

        out
    }
}

#[cfg(test)]
mod tests {
    use crate::ast::Expression::{Identifier, EMPTY};
    use crate::ast::Statement::LetStatement;
    use crate::ast::{Program, Statement};
    use monkey_token::token::Token;
    use monkey_token::token::TokenType::IDENT;

    #[test]
    fn test_let_statements() {
        let program = Program {
            statements: Vec::from([LetStatement {
                identifier: Token {
                    token_type: IDENT,
                    literal: "myVar",
                },
                expression: Identifier {
                    0: Token {
                        token_type: IDENT,
                        literal: "anotherVar",
                    },
                },
            }]),
        };

        assert_eq!(program.to_string(), "let myVar = anotherVar;");
    }
}
