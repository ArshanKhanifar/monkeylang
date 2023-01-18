use monkey_token::token::Token;

pub trait HasToken {
    fn token_literal(&self) -> &str;
}

#[derive(Debug, PartialEq)]
pub enum Expression<'a> {
    Identifier(Token<'a>),
    EMPTY, // TODO: Look wherever you used this and remove it
}

impl<'a> ToString for Expression<'a> {
    fn to_string(&self) -> String {
        match self {
            Expression::Identifier(token) => token.literal.to_string(),
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
        return_value: Expression<'a>,
    },
    ExpressionStatement {
        expression: Expression<'a>,
    },
}

impl<'a> ToString for Statement<'a> {
    fn to_string(&self) -> String {
        match self {
            Statement::LetStatement {
                identifier,
                expression,
            } => format!("let {} = {};", identifier.literal, expression.to_string()).to_string(),
            Statement::ReturnStatement { return_value } => {
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
