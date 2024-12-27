#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub token_type: TokenType,
    pub linenum: usize,
    pub lexeme: String,
}

#[derive(Debug, Clone, PartialEq)]
pub enum TokenType {
    // Single-character tokens.
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    Dot,
    Minus,
    Plus,
    Semicolon,
    Slash,
    SlashSlash,
    Star,
    Question,
    Colon,

    // One or two character tokens.
    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,

    // Literals.
    Identifier(String),
    String(String),
    Number(f64),
    Boolean(bool),

    // Keywords.
    And,
    Class,
    Else,
    Fun,
    For,
    If,
    Nil,
    Or,
    Print,
    Return,
    Super,
    This,
    Let,
    While,

    Eof,
    NewLine,
}

impl std::fmt::Display for TokenType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let lexeme = match self {
            // Single-character tokens.
            Self::LeftParen => "(",
            Self::RightParen => ")",
            Self::LeftBrace => "{{",
            Self::RightBrace => "}}",
            Self::Comma => ",",
            Self::Dot => ".",
            Self::Minus => "-",
            Self::Plus => "+",
            Self::Semicolon => ";",
            Self::Slash => "/",
            Self::SlashSlash => "//",
            Self::Star => "*",
            Self::Question => "?",
            Self::Colon => ":",

            // One or two character tokens.
            Self::Bang => "!",
            Self::BangEqual => "!=",
            Self::Equal => "=",
            Self::EqualEqual => "==",
            Self::Greater => ">",
            Self::GreaterEqual => ">=",
            Self::Less => "<",
            Self::LessEqual => "<=",

            // Literals.
            Self::Identifier(ident) => ident,
            Self::String(str) => &format!("\"{}\"", str), // String lexeme will
            // include the surrounding
            // '"'
            Self::Number(num) => &num.to_string(),
            Self::Boolean(bool) => &bool.to_string(),

            // Keywords.
            Self::And => "and",
            Self::Class => "class",
            Self::Else => "else",
            Self::Fun => "fun",
            Self::For => "for",
            Self::If => "if",
            Self::Nil => "nil",
            Self::Or => "or",
            Self::Print => "print",
            Self::Return => "return",
            Self::Super => "super",
            Self::This => "this",
            Self::Let => "let",
            Self::While => "while",

            // Eof and NewLine
            Self::Eof => "EOF",
            Self::NewLine => "\\n",
        };
        write!(f, "{}", lexeme)
    }
}

impl std::fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.token_type)
    }
}

#[allow(clippy::len_without_is_empty)]
impl Token {
    pub fn new(token_type: TokenType, linenum: usize) -> Self {
        Self {
            lexeme: token_type.to_string(),
            token_type,
            linenum,
        }
    }
    pub fn len(&self) -> usize {
        self.lexeme.len()
    }
}
