use phf::{phf_map, Map};
use std::fmt::{Debug, Formatter, Result};

pub type TokenType = &'static str;

pub const ILLEGAL: TokenType = "ILLEGAL";
pub const EOF: TokenType = "EOF";
pub const IDENT: TokenType = "IDENT";
pub const INT: TokenType = "INT";

// Operators
pub const ASSIGN: TokenType = "=";
pub const PLUS: TokenType = "+";
pub const MINUS: TokenType = "-";
pub const BANG: TokenType = "!";
pub const ASTERISK: TokenType = "*";
pub const SLASH: TokenType = "/";

pub const LT: TokenType = "<";
pub const GT: TokenType = ">";

pub const EQ: TokenType = "==";
pub const NOT_EQ: TokenType = "!=";

// Misc
pub const COMMA: TokenType = ",";
pub const SEMICOLON: TokenType = ";";
pub const LPAREN: TokenType = "(";
pub const RPAREN: TokenType = ")";
pub const LBRACE: TokenType = "{";
pub const RBRACE: TokenType = "}";

// Keywords
pub const FUNCTION: TokenType = "FUNCTION";
pub const LET: TokenType = "LET";
pub const TRUE: TokenType = "TRUE";
pub const FALSE: TokenType = "FALSE";
pub const IF: TokenType = "IF";
pub const ELSE: TokenType = "ELSE";
pub const RETURN: TokenType = "RETURN";

pub struct Token<'a> {
    pub token_type: TokenType,
    pub literal: &'a str,
}

static KEYWORDS_MAP: Map<&str, TokenType> = phf_map! {
    "fn" => FUNCTION,
    "let" => LET,
    "true" => TRUE,
    "false" => FALSE,
    "if" => IF,
    "else" => ELSE,
    "return" => RETURN,
};

impl<'a> Debug for Token<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(
            f,
            "Token {{ token_type: {}, literal: {} }}",
            self.token_type, self.literal
        )
    }
}

pub fn lookup_ident(ident: &str) -> TokenType {
    KEYWORDS_MAP.get(ident).unwrap_or(&IDENT)
}
