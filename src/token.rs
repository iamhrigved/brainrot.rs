#[derive(Debug, Clone, PartialEq)]
pub enum Token {
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
}

impl std::fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let lexeme = match self {
            // Single-character tokens.
            Token::LeftParen => "(",
            Token::RightParen => ")",
            Token::LeftBrace => "{{",
            Token::RightBrace => "}}",
            Token::Comma => ",",
            Token::Dot => ".",
            Token::Minus => "-",
            Token::Plus => "+",
            Token::Semicolon => ";",
            Token::Slash => "/",
            Token::Star => "*",
            Token::Question => "?",
            Token::Colon => ":",

            // One or two character tokens.
            Token::Bang => "!",
            Token::BangEqual => "!=",
            Token::Equal => "=",
            Token::EqualEqual => "==",
            Token::Greater => ">",
            Token::GreaterEqual => ">=",
            Token::Less => "<",
            Token::LessEqual => "<=",

            // Literals.
            Token::Identifier(ident) => ident,
            Token::String(str) => &format!("\"{}\"", str), // String lexeme will
            // include the surrounding
            // '"'
            Token::Number(num) => &num.to_string(),
            Token::Boolean(bool) => &bool.to_string(),

            // Keywords.
            Token::And => "and",
            Token::Class => "class",
            Token::Else => "else",
            Token::Fun => "fun",
            Token::For => "for",
            Token::If => "if",
            Token::Nil => "nil",
            Token::Or => "or",
            Token::Print => "print",
            Token::Return => "return",
            Token::Super => "super",
            Token::This => "this",
            Token::Let => "let",
            Token::While => "while",

            // Eof
            Token::Eof => "EOF",
        };

        write!(f, "{}", lexeme)
    }
}

#[allow(clippy::len_without_is_empty)]
impl Token {
    pub fn len(&self) -> usize {
        self.to_string().len()
    }
}
