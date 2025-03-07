#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub token_type: TokenType,
    pub pos: (usize, usize),
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
    Let,
    As,
    While,
    Continue,
    Break,

    // Exceptions
    Error,
    TypeError,
    NameError,
    IndexError,
    ValueError,
    PropertyError,

    // variable arguments
    Varargs,

    // Brainrot
    Lit,      // let
    Cap,      // false
    NoCap,    // true
    RN,       // semicolon
    Spit,     // print
    FkAround, // try
    FindOut,  // catch
    Yeet,     // return
    AightBet, // if
    NahDawg,  // else
    FrTho,    // else if
    Be,       // equals
    Is,       // ==

    Eof,
}

impl std::fmt::Display for TokenType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let lexeme = match self {
            // Operators
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

            // Comparison
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
            Self::String(str) => &format!("\"{}\"", str), // String with '"'
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
            Self::Class => "class",
            Self::Meth => "meth",
            Self::Super => "super",
            Self::Let => "let",
            Self::As => "as",
            Self::While => "while",
            Self::Continue => "continue",
            Self::Break => "break",

            // Exceptions
            Self::Error => "Error",
            Self::TypeError => "TypeError",
            Self::NameError => "NameError",
            Self::IndexError => "IndexError",
            Self::ValueError => "ValueError",
            Self::PropertyError => "PropertyError",

            Self::Varargs => "...",

            // Brainrot
            Self::Lit => "lit",
            Self::Cap => "cap",
            Self::NoCap => "nocap",
            Self::RN => "rn",
            Self::Spit => "spit",
            Self::FkAround => "fuk around",
            Self::FindOut => "find out",
            Self::Yeet => "yeet",
            Self::AightBet => "aight bet",
            Self::NahDawg => "nah dawg",
            Self::FrTho => "fr tho?",
            Self::Be => "be",
            Self::Is => "is",

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
        Self { token_type, pos }
    }
    pub fn get_second_half(identifier: &str) -> Option<&str> {
        let first_half = ["fuk", "find", "aight", "nah", "fr"];
        let second_half = ["around", "out", "bet", "dawg", "tho?"];

        let pos = first_half.iter().position(|first| *first == identifier)?;

        second_half.get(pos).copied()
    }
}
