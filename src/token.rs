#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub token_type: TokenType,
    pub pos: (usize, usize),
    pub lexeme: String,
}

#[derive(Debug, Clone, PartialEq)]
pub enum TokenType {
    // Arithmetic and other operators
    LeftParen,
    RightParen,
    LeftBracket,
    RightBracket,
    LeftBrace,
    RightBrace,
    Comma,
    Dot,
    Minus,
    MinusMinus,
    Plus,
    PlusPlus,
    Slash,
    SlashSlash,
    Star,
    StarStar,
    Modulus,
    Question,
    Colon,
    Semicolon,

    // Comparison operators
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
    TypeLiteral(String),

    // Keywords.
    And,
    Else,
    Fun,
    For,
    In,
    If,
    Nil,
    Or,
    Try,
    Catch,
    Throw,
    Print,
    Return,
    Class,
    Meth,
    Super,
    //Me,
    Let,
    As,
    While,
    Continue,
    Break,

    // Exceptions
    Exception,
    TypeError,
    NameError,
    IndexError,
    ValueError,
    PropertyError,

    // variable arguments
    Varargs,

    Eof,
}

impl std::fmt::Display for TokenType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let lexeme = match self {
            // Single-character tokens.
            Self::LeftParen => "(",
            Self::RightParen => ")",
            Self::LeftBracket => "[",
            Self::RightBracket => "]",
            Self::LeftBrace => "{",
            Self::RightBrace => "}",
            Self::Comma => ",",
            Self::Dot => ".",
            Self::Minus => "-",
            Self::MinusMinus => "--",
            Self::Plus => "+",
            Self::PlusPlus => "++",
            Self::Semicolon => ";",
            Self::Slash => "/",
            Self::SlashSlash => "//",
            Self::Star => "*",
            Self::StarStar => "**",
            Self::Modulus => "%",
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
            Self::Identifier(ident) => &ident.to_string(),
            Self::String(str) => &format!("\"{}\"", str), // String lexeme will
            // include the surrounding
            // '"'
            Self::Number(num) => &num.to_string(),
            Self::Boolean(bool) => &bool.to_string(),
            Self::TypeLiteral(type_name) => type_name,

            // Keywords.
            Self::And => "and",
            Self::Else => "else",
            Self::Fun => "fun",
            Self::For => "for",
            Self::In => "in",
            Self::If => "if",
            Self::Nil => "nil",
            Self::Or => "or",
            Self::Try => "try",
            Self::Catch => "catch",
            Self::Throw => "throw",
            Self::Print => "print",
            Self::Return => "return",
            Self::Class => "type",
            Self::Meth => "meth",
            Self::Super => "super",
            //Self::Me => "this",
            Self::Let => "let",
            Self::As => "as",
            Self::While => "while",
            Self::Continue => "continue",
            Self::Break => "break",

            // Exceptions
            Self::Exception => "Exception",
            Self::TypeError => "TypeError",
            Self::NameError => "NameError",
            Self::IndexError => "IndexError",
            Self::ValueError => "ValueError",
            Self::PropertyError => "PropertyError",

            Self::Varargs => "...",

            Self::Eof => "%",
        };
        write!(f, "{}", lexeme)
    }
}

impl std::fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.token_type)
    }
}

impl Token {
    pub fn new(token_type: TokenType, pos: (usize, usize)) -> Self {
        Self {
            lexeme: token_type.to_string(),
            token_type,
            pos,
        }
    }
}
