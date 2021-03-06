pub mod prelude {
    pub use super::{
        Bool, Comment, CommentExt, Ident, IdentExt, Keyword, Number, NumberExt, Punct,
        StringLit, StringLitExt, Template, TemplateExt, Token, TokenExt, 
        NumberKind
    };
}

use std::str::FromStr;

#[derive(PartialEq, Clone, Debug)]
/// The representation of any single
/// JS part
pub enum Token<T> {
    /// `true` of `false`
    Bool(Bool),
    /// The end of the file
    EoF,
    /// An identifier this will be either a variable name
    /// or a function/method name
    Ident(Ident<T>),
    /// A word that has been reserved to not be used as an identifier
    Keyword(Keyword),
    /// A `null` literal value
    Null,
    /// A `__null` literal value
    UnsafeNull,
    /// A number, this includes integers (`1`), decimals (`0.1`),
    /// hex (`0x8f`), binary (`0b010011010`), and octal (`0o273`)
    Number(Number<T>),
    /// A punctuation mark, this includes all mathematical operators
    /// logical operators and general syntax punctuation
    Punct(Punct),
    /// A string literal, either double or single quoted, the associated
    /// value will be the unquoted string
    String(StringLit<T>),
    /// The string parts of a template string
    Template(Template<T>),
    /// A comment, the associated value will contain the raw comment
    /// This will capture inline comments `// I am an inline comment`,
    /// multi-line comments, HTML-style comments 
    /// ```js
    /// /*multi lines
    /// * comments
    /// */
    /// ```
    Comment(Comment<T>),
}

/// Extension methods for
/// implementing allowing Token
/// to work with both &str and String
pub trait TokenExt {
    fn is_boolean(&self) -> bool;

    fn is_boolean_true(&self) -> bool;

    fn is_boolean_false(&self) -> bool;

    fn is_eof(&self) -> bool;

    fn is_ident(&self) -> bool;

    fn is_keyword(&self) -> bool;

    fn is_null(&self) -> bool;

    fn is_number(&self) -> bool;

    fn is_hex_literal(&self) -> bool;

    fn is_bin_literal(&self) -> bool;

    fn is_oct_literal(&self) -> bool;

    fn is_punct(&self) -> bool;

    fn is_string(&self) -> bool;

    fn is_double_quoted_string(&self) -> bool;

    fn is_single_quoted_string(&self) -> bool;

    fn is_template(&self) -> bool;

    fn is_template_no_sub(&self) -> bool;

    fn is_template_head(&self) -> bool;

    fn is_template_body(&self) -> bool;

    fn is_template_tail(&self) -> bool;

    fn is_literal(&self) -> bool;

    fn is_comment(&self) -> bool;

    fn is_multi_line_comment(&self) -> bool;

    fn is_single_line_comment(&self) -> bool;

    fn matches_boolean(&self, b: Bool) -> bool;

    fn matches_boolean_str(&self, b: &str) -> bool;

    fn matches_ident_str(&self, name: &str) -> bool;

    fn matches_keyword(&self, keyword: Keyword) -> bool;

    fn matches_keyword_str(&self, name: &str) -> bool;

    fn matches_number_str(&self, number: &str) -> bool;

    fn matches_punct(&self, p: Punct) -> bool;

    fn matches_punct_str(&self, s: &str) -> bool;

    fn matches_comment_str(&self, comment: &str) -> bool;

    fn matches_string_content(&self, content: &str) -> bool;
}

impl<'a> ToString for Token<&'a str> {
    fn to_string(&self) -> String {
        match self {
            Token::Bool(ref b) => b.to_string(),
            Token::Comment(ref c) => c.to_string(),
            Token::EoF => String::new(),
            Token::Ident(ref i) => i.to_string(),
            Token::Keyword(ref k) => k.to_string(),
            Token::Null => "null".to_string(),
            Token::UnsafeNull => "__null".to_string(),
            Token::Number(ref n) => n.to_string(),
            Token::Punct(ref p) => p.to_string(),
            Token::String(ref s) => s.to_string(),
            Token::Template(ref t) => t.to_string(),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
/// An identifier
pub struct Ident<T>(T);
/// Extension methods for allowing Ident
/// to work with both &str and String
pub trait IdentExt<T>
where
    T: ?Sized,
{
    fn matches(&self, other: &T) -> bool;
    fn as_str(&self) -> &str;
}

impl<'a> PartialEq<str> for Ident<&'a str> {
    fn eq(&self, other: &str) -> bool {
        self.0.eq(other)
    }
}

impl<'a> IdentExt<str> for Ident<&'a str> {
    fn matches(&self, other: &str) -> bool {
        self.0 == other
    }

    fn as_str(&self) -> &str {
        self.0
    }
}

impl IdentExt<String> for Ident<String> {
    fn matches(&self, other: &String) -> bool {
        &self.0 == other
    }

    fn as_str(&self) -> &str {
        &self.0
    }
}

impl<'a> From<&'a str> for Ident<&'a str> {
    fn from(s: &'a str) -> Self {
        Ident(s)
    }
}

impl<T> ToString for Ident<T>
where
    T: ToString,
{
    fn to_string(&self) -> String {
        self.0.to_string()
    }
}

impl<T> Into<String> for Ident<T>
where
    T: ToString,
{
    fn into(self) -> String {
        self.0.to_string()
    }
}

#[derive(Debug, PartialEq, Clone)]
/// A comment, effectively should be treated
/// as white space. There are 3 kinds of comments
/// according to the specification.
///
/// - Single line comments: //comment
/// - Multi line comments: /* comment */
/// - HTML comments: <!-- comment --> plus more!
pub struct Comment<T> {
    pub kind: CommentKind,
    pub content: T,
    pub tail_content: Option<T>,
}
/// Extension methods for comment
/// to work with both &str and String
pub trait CommentExt<T> {
    fn from_parts(content: T, kind: CommentKind, tail_content: Option<T>) -> Comment<T>;
    fn new_single_line(content: T) -> Comment<T>;
    fn new_multi_line(content: T) -> Comment<T>;
    fn new_html(content: T, tail_content: Option<T>) -> Comment<T>;
    fn new_html_no_tail(content: T) -> Comment<T>;
    fn new_html_with_tail(content: T, tail: T) -> Comment<T>;
    fn is_multi_line(&self) -> bool;
    fn is_single_line(&self) -> bool;
    fn is_html(&self) -> bool;
}

impl<'a> CommentExt<&'a str> for Comment<&'a str> {
    fn from_parts(content: &'a str, kind: CommentKind, tail_content: Option<&'a str>) -> Self {
        Comment {
            content,
            kind,
            tail_content,
        }
    }

    fn new_single_line(content: &'a str) -> Self {
        Comment::from_parts(content, CommentKind::Single, None)
    }

    fn new_multi_line(content: &'a str) -> Self {
        Comment::from_parts(content, CommentKind::Multi, None)
    }

    fn new_html(content: &'a str, tail_content: Option<&'a str>) -> Self {
        Comment::from_parts(content, CommentKind::Html, tail_content)
    }

    fn new_html_no_tail(content: &'a str) -> Self {
        Comment::new_html(content, None)
    }

    fn new_html_with_tail(content: &'a str, tail: &'a str) -> Self {
        Comment::new_html(content, Some(tail))
    }

    fn is_multi_line(&self) -> bool {
        self.kind == CommentKind::Multi
    }

    fn is_single_line(&self) -> bool {
        self.kind == CommentKind::Single
    }

    fn is_html(&self) -> bool {
        self.kind == CommentKind::Multi
    }
}
impl CommentExt<String> for Comment<String> {
    fn from_parts(content: String, kind: CommentKind, tail_content: Option<String>) -> Self {
        Comment {
            content: content,
            kind,
            tail_content: tail_content,
        }
    }

    fn new_single_line(content: String) -> Self {
        Comment::from_parts(content, CommentKind::Single, None)
    }

    fn new_multi_line(content: String) -> Self {
        Comment::from_parts(content, CommentKind::Multi, None)
    }

    fn new_html(content: String, tail_content: Option<String>) -> Self {
        Comment::from_parts(content, CommentKind::Html, tail_content)
    }

    fn new_html_no_tail(content: String) -> Self {
        Comment::new_html(content, None)
    }

    fn new_html_with_tail(content: String, tail: String) -> Self {
        Comment::new_html(content, Some(tail))
    }

    fn is_multi_line(&self) -> bool {
        self.kind == CommentKind::Multi
    }

    fn is_single_line(&self) -> bool {
        self.kind == CommentKind::Single
    }

    fn is_html(&self) -> bool {
        self.kind == CommentKind::Multi
    }
}

impl ToString for Comment<String> {
    fn to_string(&self) -> String {
        match self.kind {
            CommentKind::Single => format!("//{}", self.content),
            CommentKind::Multi => format!("/*{}*/", self.content),
            CommentKind::Html => format!("<!--{}-->", self.content),
        }
    }
}
impl ToString for Comment<&str> {
    fn to_string(&self) -> String {
        match self.kind {
            CommentKind::Single => format!("//{}", self.content),
            CommentKind::Multi => format!("/*{}*/", self.content),
            CommentKind::Html => format!("<!--{}-->", self.content),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
/// A JS number literal. There are 4 kinds of number
/// literals allowed in JS.
///
/// - Decimal Literals - This includes integers and decimals with
///     optional exponent notation
/// - Hexadecimal Literals - These begin with 0x and consist of numbers
///     0-9 and letters A-F (case insensitive)
/// - Octal Literals - These being with 0o and consist of numbers
///     0-7
/// - Binary Literals - These begin with 0b and consist of numbers 0 and 1
pub struct Number<T>(T);

/// Extension methods for allowing Number
/// to work with both &str and String
pub trait NumberExt {
    fn kind(&self) -> NumberKind;
    fn is_hex(&self) -> bool;
    fn is_bin(&self) -> bool;
    fn is_oct(&self) -> bool;
    fn is_dec(&self) -> bool;
    fn is_deci(&self) -> bool;
    fn is_decf(&self) -> bool;
    fn has_exponent(&self) -> bool;
    fn parse_f64(&self) -> Result<f64, <f64 as FromStr>::Err>;
    fn parse_i128(&self) -> Result<i128, <i128 as FromStr>::Err>;
    
    fn str_len(&self) -> usize;
}

impl<'a> NumberExt for Number<&'a str> {
    fn kind(&self) -> NumberKind {
        if self.0.starts_with("0x") {
            NumberKind::Hex
        } else if self.0.starts_with("0b") {
            NumberKind::Bin
        } else if self.0.starts_with("0o") {
            NumberKind::Oct
        } else if self.0.contains(".") {
            NumberKind::DecF
        } else {
            NumberKind::DecI
        }
    }

    fn is_hex(&self) -> bool {
        self.kind() == NumberKind::Hex
    }
    fn is_bin(&self) -> bool {
        self.kind() == NumberKind::Bin
    }
    fn is_oct(&self) -> bool {
        self.kind() == NumberKind::Oct
    }
    fn is_deci(&self) -> bool {
        self.kind() == NumberKind::DecI
    }
    fn is_decf(&self) -> bool {
        self.kind() == NumberKind::DecF
    }
    fn is_dec(&self) -> bool {
        self.is_decf() || self.is_deci()
    }
    fn has_exponent(&self) -> bool {
        match self.kind() {
            NumberKind::DecI => self.0.contains(|c| c == 'e' || c == 'E'),
            NumberKind::DecF => self.0.contains(|c| c == 'e' || c == 'E'),
            _ => false,
        }
    }

    fn parse_i128(&self) -> Result<i128, <i128 as FromStr>::Err> {
        match self.kind() {
            NumberKind::DecF | NumberKind::DecI => self.0.parse(),
            NumberKind::Hex => u128::from_str_radix(self.0.trim_start_matches("0x"), 16).map(|i| i as i128),
            NumberKind::Oct => u128::from_str_radix(self.0.trim_start_matches("0o"), 8).map(|i| i as i128),
            NumberKind::Bin => u128::from_str_radix(self.0.trim_start_matches("0b"), 2).map(|i| i as i128),
        }
    }

    fn parse_f64(&self) -> Result<f64, <f64 as FromStr>::Err> {
        self.0.parse()
    }

    fn str_len(&self) -> usize {
        self.0.len()
    }
}
impl NumberExt for Number<String> {
    fn kind(&self) -> NumberKind {
        if self.0.starts_with("0x") {
            NumberKind::Hex
        } else if self.0.starts_with("0b") {
            NumberKind::Bin
        } else if self.0.starts_with("0o") {
            NumberKind::Oct
        } else if self.0.contains(".") {
            NumberKind::DecF
        } else {
            NumberKind::DecI
        }
    }

    fn is_hex(&self) -> bool {
        self.kind() == NumberKind::Hex
    }
    fn is_bin(&self) -> bool {
        self.kind() == NumberKind::Bin
    }
    fn is_oct(&self) -> bool {
        self.kind() == NumberKind::Oct
    }
    fn is_deci(&self) -> bool {
        self.kind() == NumberKind::DecI
    }
    fn is_decf(&self) -> bool {
        self.kind() == NumberKind::DecF
    }
    fn is_dec(&self) -> bool {
        self.is_decf() || self.is_deci()
    }
    fn has_exponent(&self) -> bool {
        match self.kind() {
            NumberKind::DecI => self.0.contains(|c| c == 'e' || c == 'E'),
            NumberKind::DecF => self.0.contains(|c| c == 'e' || c == 'E'),
            _ => false,
        }
    }

    fn parse_i128(&self) -> Result<i128, <i128 as FromStr>::Err> {
        match self.kind() {
            NumberKind::DecF | NumberKind::DecI => self.0.parse(),
            NumberKind::Hex => u128::from_str_radix(self.0.as_str().trim_start_matches("0x"), 16).map(|i| i as i128),
            NumberKind::Oct => u128::from_str_radix(self.0.as_str().trim_start_matches("0o"), 8).map(|i| i as i128),
            NumberKind::Bin => u128::from_str_radix(self.0.as_str().trim_start_matches("0b"), 2).map(|i| i as i128),
        }
    }

    fn parse_f64(&self) -> Result<f64, <f64 as FromStr>::Err> {
        self.0.parse()
    }

    fn str_len(&self) -> usize {
        self.0.len()
    }
}

impl<'a> From<&'a str> for Number<&'a str> {
    fn from(s: &'a str) -> Self {
        Number(s)
    }
}

impl<'a> ToString for Number<&'a str> {
    fn to_string(&self) -> String {
        self.0.to_string()
    }
}

impl<'a> PartialEq<str> for Number<&'a str> {
    fn eq(&self, other: &str) -> bool {
        self.0.eq(other)
    }
}

#[derive(Debug, PartialEq, Clone)]
/// A single or double quoted string
/// literal
pub enum StringLit<T> {
    Single(T),
    Double(T),
}

/// Extension methods for allowing StringLit
/// to work with both &str and String
pub trait StringLitExt<T> {
    fn single(content: T) -> StringLit<T>;
    fn double(content: T) -> StringLit<T>;
    fn is_single(&self) -> bool;
    fn is_double(&self) -> bool;
    fn no_quote(&self) -> T;
}

impl<T> ToString for StringLit<T>
where
    T: ::core::fmt::Display,
{
    fn to_string(&self) -> String {
        match self {
            StringLit::Single(ref s) => format!(r#"'{}'"#, s),
            StringLit::Double(ref s) => format!(r#""{}""#, s),
        }
    }
}

impl<'a> StringLitExt<&'a str> for StringLit<&'a str> {
    fn single(content: &'a str) -> Self {
        StringLit::Single(content)
    }
    fn double(content: &'a str) -> Self {
        StringLit::Double(content)
    }
    fn is_single(&self) -> bool {
        match self {
            StringLit::Single(_) => true,
            _ => false,
        }
    }
    fn is_double(&self) -> bool {
        match self {
            StringLit::Double(_) => true,
            _ => false,
        }
    }
    fn no_quote(&self) -> &'a str {
        match self {
            StringLit::Single(ref inner) => inner,
            StringLit::Double(ref inner) => inner,
        }
    }
}
impl StringLitExt<String> for StringLit<String> {
    fn single(content: String) -> Self {
        StringLit::Single(content)
    }
    fn double(content: String) -> Self {
        StringLit::Double(content)
    }
    fn is_single(&self) -> bool {
        match self {
            StringLit::Single(_) => true,
            _ => false,
        }
    }
    fn is_double(&self) -> bool {
        match self {
            StringLit::Double(_) => true,
            _ => false,
        }
    }
    fn no_quote(&self) -> String {
        match self {
            StringLit::Single(ref inner) => inner.clone(),
            StringLit::Double(ref inner) => inner.clone(),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
/// A template string
///
/// These include strings that are wrapped in back ticks (`)
/// which allows for interpolating any js expression between `${`
/// and `}`
pub enum Template<T> {
    NoSub(T),
    Head(T),
    Middle(T),
    Tail(T),
}
/// Extension methods for allowing Template
/// to work with both &str and String
pub trait TemplateExt<T> {
    fn no_sub_template(content: T) -> Template<T>;
    fn template_head(content: T) -> Template<T>;
    fn template_middle(content: T) -> Template<T>;
    fn template_tail(content: T) -> Template<T>;
    fn is_head(&self) -> bool;
    fn is_middle(&self) -> bool;
    fn is_tail(&self) -> bool;
    fn is_no_sub(&self) -> bool;
}

impl<'a> TemplateExt<&'a str> for Template<&'a str> {
    fn no_sub_template(content: &'a str) -> Self {
        Template::NoSub(content)
    }
    fn template_head(content: &'a str) -> Self {
        Template::Head(content)
    }
    fn template_middle(content: &'a str) -> Self {
        Template::Middle(content)
    }
    fn template_tail(content: &'a str) -> Self {
        Template::Tail(content)
    }
    fn is_head(&self) -> bool {
        match self {
            Template::Head(_) => true,
            _ => false,
        }
    }
    fn is_middle(&self) -> bool {
        match self {
            Template::Middle(_) => true,
            _ => false,
        }
    }
    fn is_tail(&self) -> bool {
        match self {
            Template::Tail(_) => true,
            _ => false,
        }
    }
    fn is_no_sub(&self) -> bool {
        match self {
            Template::NoSub(_) => true,
            _ => false,
        }
    }
}
impl TemplateExt<String> for Template<String> {
    fn no_sub_template(content: String) -> Self {
        Template::NoSub(content)
    }
    fn template_head(content: String) -> Self {
        Template::Head(content)
    }
    fn template_middle(content: String) -> Self {
        Template::Middle(content)
    }
    fn template_tail(content: String) -> Self {
        Template::Tail(content)
    }
    fn is_head(&self) -> bool {
        match self {
            Template::Head(_) => true,
            _ => false,
        }
    }
    fn is_middle(&self) -> bool {
        match self {
            Template::Middle(_) => true,
            _ => false,
        }
    }
    fn is_tail(&self) -> bool {
        match self {
            Template::Tail(_) => true,
            _ => false,
        }
    }
    fn is_no_sub(&self) -> bool {
        match self {
            Template::NoSub(_) => true,
            _ => false,
        }
    }
}

impl<T> ToString for Template<T>
where
    T: ::core::fmt::Display,
{
    fn to_string(&self) -> String {
        match self {
            Template::NoSub(ref c) => format!("`{}`", c),
            Template::Head(ref c) => format!("`{}${{", c),
            Template::Middle(ref c) => format!("}}{}${{", c),
            Template::Tail(ref c) => format!("}}{}`", c),
        }
    }
}

#[derive(Debug, PartialEq, Clone, Copy)]
/// The tokenized representation of `true` or `false`
pub enum Bool {
    True,
    False,
}
impl Bool {
    /// Test if this instance represents `true`
    pub fn is_true(self) -> bool {
        match self {
            Bool::True => true,
            _ => false,
        }
    }
}

impl Bool {
    /// Create a Bool from raw text
    pub fn from(s: &str) -> Option<Self> {
        if s == "true" {
            Some(Bool::True)
        } else if s == "false" {
            Some(Bool::False)
        } else {
            None
        }
    }
}

impl From<bool> for Bool {
    /// Creates a JS Bool for a rust bool
    fn from(b: bool) -> Self {
        if b {
            Bool::True
        } else {
            Bool::False
        }
    }
}

impl Into<String> for Bool {
    /// Return this Bool to the text
    /// that was parsed to create it
    fn into(self) -> String {
        match self {
            Bool::True => "true".into(),
            Bool::False => "false".into(),
        }
    }
}

impl ToString for Bool {
    /// Return this Bool to the text
    /// that was parsed to create it
    fn to_string(&self) -> String {
        match self {
            Bool::True => "true".into(),
            Bool::False => "false".into(),
        }
    }
}

impl Into<bool> for Bool {
    /// Creates a Rust bool for a js bool
    fn into(self) -> bool {
        match self {
            Bool::True => true,
            Bool::False => false,
        }
    }
}

impl<'a> Into<bool> for &'a Bool {
    /// Creates a js bool for a rust bool
    fn into(self) -> bool {
        match self {
            Bool::True => true,
            Bool::False => false,
        }
    }
}

#[derive(Debug, PartialEq, Clone, Copy)]
/// The 5 kinds of numbers
pub enum NumberKind {
    DecF,
    DecI,
    Hex,
    Bin,
    Oct,
}

#[derive(Debug, PartialEq, Clone, Copy)]
/// All available punctuation
pub enum Punct {
    Ampersand,
    AmpersandEqual,
    /// The fat arrow, '=>'
    FatArrow,
    /// The thin arrow, '->'
    ThinArrow,
    Asterisk,
    AsteriskEqual,
    AtMark,
    Bang,
    BangDoubleEqual,
    BangEqual,
    Caret,
    CaretEqual,
    CloseBrace,
    /// ']'
    CloseBracket,
    CloseParen,
    Colon,
    Comma,
    Dash,
    DoubleDash,
    DashEqual,
    DoubleAmpersand,
    DoubleAsterisk,
    DoubleAsteriskEqual,
    DoubleEqual,
    DoublePipe,
    DoublePlus,
    Ellipsis,
    Equal,
    ForwardSlash,
    ForwardSlashEqual,
    GreaterThan,
    GreaterThanEqual,
    Hash,
    LessThan,
    LessThanEqual,
    /// A '{' 
    OpenBrace,
    /// A '[' 
    OpenBracket,
    /// A '(' 
    OpenParen,
    Percent,
    PercentEqual,
    Period,
    Pipe,
    PipeEqual,
    Plus,
    PlusEqual,
    QuestionMark,
    SemiColon,
    Tilde,
    TripleEqual,
}

impl Punct {
    fn matches_str(self, s: &str) -> bool {
        match self {
            Punct::OpenBrace => "{" == s,
            Punct::CloseBrace => "}" == s,
            Punct::OpenParen => "(" == s,
            Punct::CloseParen => ")" == s,
            Punct::Period => "." == s,
            Punct::SemiColon => ";" == s,
            Punct::Comma => "," == s,
            Punct::OpenBracket => "[" == s,
            Punct::CloseBracket => "]" == s,
            Punct::Colon => ":" == s,
            Punct::QuestionMark => "?" == s,
            Punct::Tilde => "~" == s,
            Punct::GreaterThan => ">" == s,
            Punct::LessThan => "<" == s,
            Punct::Equal => "=" == s,
            Punct::Bang => "!" == s,
            Punct::Plus => "+" == s,
            Punct::Dash => "-" == s,
            Punct::Asterisk => "*" == s,
            Punct::Percent => "%" == s,
            Punct::Pipe => "|" == s,
            Punct::Ampersand => "&" == s,
            Punct::Caret => "^" == s,
            Punct::ForwardSlash => "/" == s,
            Punct::Ellipsis => "..." == s,
            Punct::TripleEqual => "===" == s,
            Punct::BangDoubleEqual => "!==" == s,
            Punct::DoubleAsteriskEqual => "**=" == s,
            Punct::DoubleAmpersand => "&&" == s,
            Punct::DoublePipe => "||" == s,
            Punct::DoubleEqual => "==" == s,
            Punct::BangEqual => "!=" == s,
            Punct::PlusEqual => "+=" == s,
            Punct::DashEqual => "-=" == s,
            Punct::AsteriskEqual => "*=" == s,
            Punct::ForwardSlashEqual => "/=" == s,
            Punct::DoublePlus => "++" == s,
            Punct::DoubleDash => "--" == s,
            Punct::AmpersandEqual => "&=" == s,
            Punct::PipeEqual => "|=" == s,
            Punct::CaretEqual => "^=" == s,
            Punct::PercentEqual => "%=" == s,
            Punct::FatArrow => "=>" == s,
            Punct::ThinArrow => "->" == s,
            Punct::GreaterThanEqual => ">=" == s,
            Punct::LessThanEqual => "<=" == s,
            Punct::DoubleAsterisk => "**" == s,
            Punct::Hash => "#" == s,
            Punct::AtMark => "@" == s,
        }
    }

    
}

impl ToString for Punct {
    fn to_string(&self) -> String {
        match self {
            Punct::OpenBrace => "{",
            Punct::CloseBrace => "}",
            Punct::OpenParen => "(",
            Punct::CloseParen => ")",
            Punct::Period => ".",
            Punct::SemiColon => ";",
            Punct::Comma => ",",
            Punct::OpenBracket => "[",
            Punct::CloseBracket => "]",
            Punct::Colon => ":",
            Punct::QuestionMark => "?",
            Punct::Tilde => "~",
            Punct::GreaterThan => ">",
            Punct::LessThan => "<",
            Punct::Equal => "=",
            Punct::Bang => "!",
            Punct::Plus => "+",
            Punct::Dash => "-",
            Punct::Asterisk => "*",
            Punct::Percent => "%",
            Punct::Pipe => "|",
            Punct::Ampersand => "&",
            Punct::Caret => "^",
            Punct::ForwardSlash => "/",
            Punct::Ellipsis => "...",
            Punct::TripleEqual => "===",
            Punct::BangDoubleEqual => "!==",
            Punct::DoubleAsteriskEqual => "**=",
            Punct::DoubleAmpersand => "&&",
            Punct::DoublePipe => "||",
            Punct::DoubleEqual => "==",
            Punct::BangEqual => "!=",
            Punct::PlusEqual => "+=",
            Punct::DashEqual => "-=",
            Punct::AsteriskEqual => "*=",
            Punct::ForwardSlashEqual => "/=",
            Punct::DoublePlus => "++",
            Punct::DoubleDash => "--",
            Punct::AmpersandEqual => "&=",
            Punct::PipeEqual => "|=",
            Punct::CaretEqual => "^=",
            Punct::PercentEqual => "%=",
            Punct::FatArrow => "=>",
            Punct::ThinArrow => "->",
            Punct::GreaterThanEqual => ">=",
            Punct::LessThanEqual => "<=",
            Punct::DoubleAsterisk => "**",
            Punct::Hash => "#",
            Punct::AtMark => "@",
        }
        .into()
    }
}

#[derive(Debug, PartialEq, Clone, Copy)]
/// The 4 kinds of comments
pub enum CommentKind {
    Single,
    Multi,
    Html,
}

#[derive(Debug, PartialEq, Clone, Copy)]
/// A Keyword
pub enum Keyword {
    Await,
    Break,
    Case,
    Catch,
    Class,
    Continue,
    Debugger,
    Default,
    Delete,
    Do,
    Else,
    Enum,
    Export,
    Finally,
    For,
    From,
    Fn,
    If,
    Implement,
    Implements,
    Import,
    In,
    InstanceOf,
    Interface,
    Let,
    New,
    Package,
    Private,
    Protected,
    Public,
    Return,
    Static,
    Super,
    Switch,
    This,
    Throw,
    Trait,
    Try,
    TypeOf,
    Var,
    Void,
    While,
    With,
    Yield,
    As,
    Async,
    Constructor,
    Extends,
    Any,
    Array,
    Bool,
    Never,
    Number,
    Object,
    String,
    Tuple,
    Undefined,
    Unknown,
    Int,
    UnsafePtr,
    UnsafeStruct,
    UnsafeOption,
    UnsafeSome,
    Option,
    Some,
    UnsafeArray,
    UnsafeStatic,
    UnsafeTypeGuard,
    Alias,
    Type,
    Mut,
}

impl Keyword {
    /// convert a &str into a Keyword
    pub fn from(s: &str) -> Option<Self> {
        Some(match s {
            "alias" => Keyword::Alias,
            "await" => Keyword::Await,
            "break" => Keyword::Break,
            "case" => Keyword::Case,
            "catch" => Keyword::Catch,
            "class" => Keyword::Class,
            //"const" => Keyword::Const,
            "continue" => Keyword::Continue,
            "debugger" => Keyword::Debugger,
            "default" => Keyword::Default,
            "delete" => Keyword::Delete,
            "do" => Keyword::Do,
            "else" => Keyword::Else,
            "finally" => Keyword::Finally,
            "for" => Keyword::For,
            "fn" => Keyword::Fn,
            "from" => Keyword::From,
            "if" => Keyword::If,
            "Int" => Keyword::Int,
            "instanceof" => Keyword::InstanceOf,
            "in" => Keyword::In,
            "mut" => Keyword::Mut,
            "new" => Keyword::New,
            "return" => Keyword::Return,
            "switch" => Keyword::Switch,
            "this" => Keyword::This,
            "throw" => Keyword::Throw,
            "try" => Keyword::Try,
            "typeof" => Keyword::TypeOf,
            "var" => Keyword::Var,
            "Void" => Keyword::Void,
            "while" => Keyword::While,
            "with" => Keyword::With,
            "export" => Keyword::Export,
            "import" => Keyword::Import,
            "super" => Keyword::Super,
            "enum" => Keyword::Enum,
            "implement" => Keyword::Implement,
            "implements" => Keyword::Implements,
            "interface" => Keyword::Interface,
            "package" => Keyword::Package,
            "private" => Keyword::Private,
            "protected" => Keyword::Protected,
            "public" => Keyword::Public,
            "static" => Keyword::Static,
            "yield" => Keyword::Yield,
            "let" => Keyword::Let,
            "as" => Keyword::As,
            "async" => Keyword::Async,
            "extends" => Keyword::Extends,
            "constructor" => Keyword::Constructor,
            "any" => Keyword::Any,
            "Array" => Keyword::Array,
            "Bool" => Keyword::Bool,
            "Never" => Keyword::Never,
            "Number" => Keyword::Number,
            "object" => Keyword::Object,
            "String" => Keyword::String,
            "Tuple" => Keyword::Tuple,
            "Undefined" => Keyword::Undefined,
            "Unknown" => Keyword::Unknown,
            "__Ptr" => Keyword::UnsafePtr,
            "__struct" => Keyword::UnsafeStruct,
            "__Option" => Keyword::UnsafeOption,
            "Option" => Keyword::Option,
            "Some" => Keyword::Some,
            "__Some" => Keyword::UnsafeSome,
            "__Array" => Keyword::UnsafeArray,
            "__static" => Keyword::UnsafeStatic,
            "__typeguard" => Keyword::UnsafeTypeGuard,
            "type" => Keyword::Type,
            "trait"  => Keyword::Trait,
            _ => return None,
        })
    }
}

impl ::std::string::ToString for Keyword {
    /// Convert a keyword into a string
    fn to_string(&self) -> String {
        self.as_str().into()
    }
}

impl Keyword {
    pub fn as_str(&self) -> &str {
        match self {
            Keyword::Await => "await",
            Keyword::Alias => "alias",
            Keyword::Break => "break",
            Keyword::Case => "case",
            Keyword::Catch => "catch",
            Keyword::Class => "class",
            //Keyword::Const => "const",
            Keyword::Continue => "continue",
            Keyword::Debugger => "debugger",
            Keyword::Default => "default",
            Keyword::Import => "import",
            Keyword::Delete => "delete",
            Keyword::Do => "do",
            Keyword::Else => "else",
            Keyword::Enum => "enum",
            Keyword::Export => "export",
            Keyword::Finally => "finally",
            Keyword::For => "for",
            Keyword::From => "from",
            Keyword::Fn => "fn",
            Keyword::If => "if",
            Keyword::In => "in",
            Keyword::Int => "Int",
            Keyword::Implement => "implement",
            Keyword::Implements => "implements",
            Keyword::InstanceOf => "instanceof",
            Keyword::Interface => "interface",
            Keyword::Let => "let",
            Keyword::Mut => "mut",
            Keyword::New => "new",
            Keyword::Package => "package",
            Keyword::Private => "private",
            Keyword::Protected => "protected",
            Keyword::Public => "public",
            Keyword::Static => "static",
            Keyword::Return => "return",
            Keyword::Super => "super",
            Keyword::Switch => "switch",
            Keyword::This => "this",
            Keyword::Throw => "throw",
            Keyword::Trait => "trait",
            Keyword::Try => "try",
            Keyword::TypeOf => "typeof",
            Keyword::Var => "var",
            Keyword::Void => "Void",
            Keyword::While => "while",
            Keyword::With => "with",
            Keyword::Yield => "yield",
            Keyword::As => "as",
            Keyword::Async => "async",
            Keyword::Extends => "extends",
            Keyword::Constructor => "constructor",
            Keyword::Any => "Any",
            Keyword::Array => "Array",
            Keyword::Bool => "Bool",
            Keyword::Never => "Never",
            Keyword::Number => "Number",
            Keyword::Object => "object",
            Keyword::String => "String",
            Keyword::Tuple => "Tuple",
            Keyword::Undefined => "Undefined",
            Keyword::Unknown => "Unknown",
            Keyword::UnsafePtr => "__Ptr",
            Keyword::UnsafeStruct => "__struct",
            Keyword::Option => "Option",
            Keyword::Some => "Some",
            Keyword::UnsafeSome => "__Some",
            Keyword::UnsafeArray => "__Array",
            Keyword::UnsafeStatic => "__static",
            Keyword::UnsafeOption => "__Option",
            Keyword::UnsafeTypeGuard => "__typeguard",
            Keyword::Type => "type",
        }
    }
}

impl<'a> TokenExt for Token<&'a str> {
    fn is_boolean(&self) -> bool {
        match self {
            Token::Bool(_) => true,
            _ => false,
        }
    }
    fn is_boolean_true(&self) -> bool {
        match self {
            Token::Bool(ref b) => b.into(),
            _ => false,
        }
    }
    fn is_boolean_false(&self) -> bool {
        match self {
            Token::Bool(ref b) => {
                let b: bool = b.into();
                !b
            }
            _ => false,
        }
    }
    fn is_eof(&self) -> bool {
        match self {
            Token::EoF => true,
            _ => false,
        }
    }
    fn is_ident(&self) -> bool {
        match self {
            Token::Ident(_) => true,
            _ => false,
        }
    }
    fn is_keyword(&self) -> bool {
        match self {
            Token::Keyword(_) => true,
            _ => false,
        }
    }
    fn is_null(&self) -> bool {
        match self {
            Token::Null => true,
            _ => false,
        }
    }

    fn is_number(&self) -> bool {
        if let Token::Number(ref _n) = self {
            true
        } else {
            false
        }
    }
    fn is_hex_literal(&self) -> bool {
        match self {
            Token::Number(ref n) => n.is_hex(),
            _ => false,
        }
    }
    fn is_bin_literal(&self) -> bool {
        match self {
            Token::Number(ref n) => n.is_bin(),
            _ => false,
        }
    }
    fn is_oct_literal(&self) -> bool {
        match self {
            Token::Number(ref n) => n.is_oct(),
            _ => false,
        }
    }
    fn is_punct(&self) -> bool {
        match self {
            Token::Punct(_) => true,
            _ => false,
        }
    }
    fn is_string(&self) -> bool {
        if let Token::String(ref _s) = self {
            true
        } else {
            false
        }
    }
    fn is_double_quoted_string(&self) -> bool {
        match self {
            Token::String(ref s) => match s {
                StringLit::Double(_) => true,
                _ => false,
            },
            _ => false,
        }
    }
    fn is_single_quoted_string(&self) -> bool {
        match self {
            Token::String(ref s) => match s {
                StringLit::Single(_) => true,
                _ => false,
            },
            _ => false,
        }
    }
    fn is_template(&self) -> bool {
        match self {
            Token::Template(_) => true,
            _ => false,
        }
    }
    fn is_template_no_sub(&self) -> bool {
        match self {
            Token::Template(ref s) => s.is_no_sub(),
            _ => false,
        }
    }
    fn is_template_head(&self) -> bool {
        match self {
            Token::Template(ref s) => s.is_head() || s.is_no_sub(),
            _ => false,
        }
    }
    fn is_template_body(&self) -> bool {
        match self {
            Token::Template(ref s) => s.is_middle(),
            _ => false,
        }
    }
    fn is_template_tail(&self) -> bool {
        match self {
            Token::Template(ref s) => s.is_tail() || s.is_no_sub(),
            _ => false,
        }
    }
    fn is_literal(&self) -> bool {
        match self {
            Token::Bool(_) => true,
            Token::String(_) => true,
            Token::Null => true,
            Token::Number(_) => true,
            Token::Template(_) => true,
            _ => false,
        }
    }
    fn is_comment(&self) -> bool {
        match self {
            Token::Comment(_) => true,
            _ => false,
        }
    }
    fn is_multi_line_comment(&self) -> bool {
        match self {
            Token::Comment(ref t) => t.kind == CommentKind::Multi,
            _ => false,
        }
    }

    fn is_single_line_comment(&self) -> bool {
        match self {
            Token::Comment(ref t) => t.kind == CommentKind::Single,
            _ => false,
        }
    }
    fn matches_boolean(&self, b: Bool) -> bool {
        match self {
            Token::Bool(m) => m == &b,
            _ => false,
        }
    }
    fn matches_boolean_str(&self, b: &str) -> bool {
        match self {
            Token::Bool(ref lit) => match (lit, b) {
                (&Bool::True, "true") | (&Bool::False, "false") => true,
                _ => false,
            },
            _ => false,
        }
    }
    fn matches_ident_str(&self, name: &str) -> bool {
        match self {
            Token::Ident(i) => i.matches(name),
            _ => false,
        }
    }
    fn matches_keyword(&self, keyword: Keyword) -> bool {
        match self {
            Token::Keyword(k) => k == &keyword,
            _ => false,
        }
    }
    fn matches_keyword_str(&self, name: &str) -> bool {
        match self {
            Token::Keyword(n) => n.as_str() == name,
            _ => false,
        }
    }
    fn matches_number_str(&self, number: &str) -> bool {
        match self {
            Token::Number(n) => n == number,
            _ => false,
        }
    }
    fn matches_punct(&self, p: Punct) -> bool {
        match self {
            Token::Punct(m) => m == &p,
            _ => false,
        }
    }

    fn matches_punct_str(&self, s: &str) -> bool {
        match self {
            Token::Punct(ref p) => p.matches_str(s),
            _ => false,
        }
    }

    fn matches_comment_str(&self, comment: &str) -> bool {
        match self {
            Token::Comment(ref t) => t.content == comment,
            _ => false,
        }
    }

    fn matches_string_content(&self, content: &str) -> bool {
        match self {
            Token::String(ref lit) => match lit {
                StringLit::Single(s) => content == *s,
                StringLit::Double(s) => content == *s,
            },
            _ => false,
        }
    }
}
