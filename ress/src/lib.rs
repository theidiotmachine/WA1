//! ress
//! A crate for parsing raw JS into a token stream
//!
//! The primary interfaces are the function [`tokenize`][tokenize] and
//! the struct [`Scanner`][scanner]. The [`Scanner`][scanner] struct impls [`Iterator`](https://doc.rust-lang.org/std/iter/trait.Iterator.html)
//! and the [`tokenize`][tokenize] function is just a wrapper
//! around [`Scanner::collect()`](https://doc.rust-lang.org/std/iter/trait.Iterator.html#method.collect).
//!
//! The `Scanner` will provide a stream of `Item`s, and `Item` is
//! has 3 properties a [`Token`][token], a [`Span`][span], and a [`SourceLocation`][location]. The `Span` is a
//! representation of where the `Item` exists in the original source while the `Token`
//! provides details about what JavaScript token it represents.
//!
//! [token]: ./enum.Token
//! [span]: ./struct.Span
//! [scanner]: ./struct.Scanner
//! [tokenize]: ../fn.tokenize
//! [location]: ./struct.SourceLocation

#[macro_use]
extern crate log;

pub mod error;
mod tokenizer;
pub mod tokens;
pub use crate::tokenizer::Tokenizer;

pub mod prelude {
    pub use super::tokenize;
    pub use super::tokens::prelude::*;
    pub use super::Item;
    pub use super::OpenCurlyKind;
    pub use super::Scanner;
    pub use super::ScannerState;
    pub use super::Span;
}
use crate::tokenizer::RawToken;
use crate::tokens::prelude::*;
use error::{Error, RawError};
use errs::prelude::*;

type Res<T> = Result<T, Error>;
mod look_behind;

use look_behind::{LookBehind};

/// a convince function for collecting a scanner into
/// a `Vec<Token>`
pub fn tokenize(text: &str) -> Res<Vec<Token<&str>>> {
    let mut ret = Vec::new();
    for i in Scanner::new(text) {
        let inner = i?.token;
        ret.push(inner);
    }
    Ok(ret)
}

#[derive(Debug, PartialEq, Clone, Copy)]
/// The start and end of a token as the byte
/// index in the original text
pub struct Span {
    pub start: usize,
    pub end: usize,
}

impl Span {
    /// Create a new Span from its parts
    #[inline]
    pub const fn new(start: usize, end: usize) -> Self {
        Self { start, end }
    }
}

#[derive(Clone, Debug, PartialEq)]
/// A single token with additional metadata
///
pub struct Item<T> {
    pub token: T,
    pub span: Span,
    pub location: SourceLocation,
}

impl<T> Item<T>
where
    T: TokenExt,
{
    pub fn new(token: T, span: Span, location: SourceLocation) -> Self {
        Self {
            token,
            span,
            location,
        }
    }
    fn new_(
        token: T,
        span_start: usize,
        span_end: usize,
        loc_start_line: usize,
        loc_start_col: usize,
        loc_end_line: usize,
        loc_end_col: usize,
    ) -> Self {
        Self {
            token,
            span: Span::new(span_start, span_end),
            location: SourceLocation::new(
                Position::new(loc_start_line, loc_start_col),
                Position::new(loc_end_line, loc_end_col),
            ),
        }
    }
    pub fn is_string(&self) -> bool {
        self.token.is_string()
    }
    pub fn is_eof(&self) -> bool {
        self.token.is_eof()
    }
    pub fn is_template(&self) -> bool {
        self.token.is_template_head()
            || self.token.is_template_body()
            || self.token.is_template_tail()
    }
}
/// The primary interface of this crate used
/// to tokenize any JS text into a stream of
/// `Item`s.
pub struct Scanner<'a> {
    pub stream: Tokenizer<'a>,
    pub eof: bool,
    pub pending_new_line: bool,
    original: &'a str,
    errored: bool,
    new_line_count: usize,
    line_cursor: usize,
    before_last_open: LookBehind,
    last_three: LookBehind,
}

impl<'a> Scanner<'a> {
    /// Create a new `Scanner` by providing the
    /// JS text
    pub fn new(text: &'a str) -> Self {
        let mut stream = Tokenizer::new(text);
        let (new_line_count, line_cursor) = stream.skip_whitespace();
        Self {
            stream,
            eof: false,
            pending_new_line: false,
            original: text,
            errored: false,
            new_line_count,
            line_cursor: usize::max(line_cursor, 1),
            before_last_open: LookBehind::new(),
            last_three: LookBehind::new(),
        }
    }
}

impl<'a> Iterator for Scanner<'a> {
    type Item = Res<Item<Token<&'a str>>>;
    fn next(&mut self) -> Option<Self::Item> {
        self.get_next_token(true)
    }
}

impl<'b> Scanner<'b> {
    /// Attempts to look ahead 1 token
    ///
    /// Similar to how `Peekable::peek` works however the
    /// returned value will not be a borrowed `Item`. Since
    /// there isn't a borrow happening this essentially duplicates
    /// the cost of calling `next`.
    pub fn look_ahead(&mut self) -> Option<Res<Item<Token<&'b str>>>> {
        self.get_next_token(false)
    }
    /// Skip any upcoming comments to get the
    /// next valid js token
    pub fn skip_comments(&mut self) -> Res<()> {
        debug!(target: "ress", "skipping comments");
        let mut new_cursor = self.stream.stream.idx;
        while let Some(item) = self.next() {
            if let Token::Comment(_) = item?.token {
                new_cursor = self.stream.stream.idx;
            } else {
                break;
            }
        }
        debug!(target: "ress", "skipped {} bytes worth of comments", new_cursor.saturating_sub(self.stream.stream.idx));
        self.stream.stream.idx = new_cursor;
        Ok(())
    }
    /// Get a copy of the scanner's current state
    pub fn get_state(&self) -> ScannerState {
        ScannerState {
            cursor: self.stream.stream.idx,
            curly_stack: self.stream.curly_stack.clone(),
            new_line_count: self.new_line_count,
            line_cursor: self.line_cursor,
            last_three: self.last_three.clone(),
            paren_three: self.before_last_open.clone(),
        }
    }
    /// Set the scanner's current state to the state provided
    #[inline]
    pub fn set_state(&mut self, state: ScannerState) {
        self.stream.stream.idx = state.cursor;
        self.stream.curly_stack = state.curly_stack;
        self.new_line_count = state.new_line_count;
        self.line_cursor = state.line_cursor;
        self.last_three = state.last_three;
        self.before_last_open = state.paren_three;
    }
    #[inline]
    /// The implementation of `Scanner::next` that includes
    /// the flag for advancing, meaning the `look_ahead` method
    /// can also use this implementation
    fn get_next_token(&mut self, advance_cursor: bool) -> Option<Res<Item<Token<&'b str>>>> {
        if self.errored {
            return None;
        }
        if self.eof {
            debug!("end of iterator, returning None");
            return None;
        };
        let prev_cursor = self.stream.stream.idx;
        let prev_lines = self.new_line_count;
        let prev_line_cursor = self.line_cursor;
        let next = match self.stream.next() {
            Ok(n) => n,
            Err(e) => {
                self.errored = true;
                return Some(self.error(e));
            }
        };
        let mut len = next.end - next.start;
        let ret = {
            let mut new_lines = 0;
            let s = &self.original[next.start..next.end];
            let token = match next.ty {
                RawToken::Bool(b) => Token::Bool(b.into()),
                RawToken::Comment {
                    kind,
                    new_line_count,
                    last_len,
                } => {
                    len = last_len;
                    new_lines = new_line_count;
                    match kind {
                        tokens::CommentKind::Multi => {
                            Token::Comment(Comment::new_multi_line(s.trim_start_matches("/*").trim_end_matches("*/")))
                        }
                        tokens::CommentKind::Single => {
                            Token::Comment(Comment::new_single_line(s.trim_start_matches("//")))
                        }
                        tokens::CommentKind::Html => {
                            let (content, tail) = if let Some(idx) = s.rfind("-->") {
                                let actual_end = idx.saturating_add(3);
                                if actual_end < next.end {
                                    let tail = &s[actual_end..];
                                    let tail = if tail == "" { None } else { Some(tail) };
                                    (&s[4..idx], tail)
                                } else {
                                    (&s[4..], None)
                                }
                            } else {
                                (&s[4..], None)
                            };
                            Token::Comment(Comment::new_html(content, tail))
                        }
                    }
                }
                RawToken::EoF => {
                    self.eof = true;
                    Token::EoF
                }
                RawToken::Ident => Token::Ident(Ident::from(s)),
                RawToken::Keyword(k) => Token::Keyword(k),
                RawToken::Null => Token::Null,
                RawToken::UnsafeNull => Token::UnsafeNull,
                RawToken::Number(_) => Token::Number(Number::from(s)),
                RawToken::Punct(p) => Token::Punct(p),
                RawToken::String {
                    kind,
                    new_line_count,
                    last_len,
                } => {
                    len = last_len;
                    new_lines = new_line_count;
                    let s = &s[1..s.len() - 1];
                    match kind {
                        tokenizer::StringKind::Double => Token::String(StringLit::Double(s)),
                        tokenizer::StringKind::Single => Token::String(StringLit::Single(s)),
                    }
                }
                RawToken::Template {
                    kind,
                    new_line_count,
                    last_len,
                } => {
                    len = last_len;
                    new_lines = new_line_count;
                    match kind {
                        tokenizer::TemplateKind::Head => {
                            let s = &s[1..s.len() - 2];
                            Token::Template(Template::Head(s))
                        }
                        tokenizer::TemplateKind::Body => {
                            let s = &s[1..s.len() - 2];
                            Token::Template(Template::Middle(s))
                        }
                        tokenizer::TemplateKind::Tail => {
                            let s = &s[1..s.len() - 1];
                            Token::Template(Template::Tail(s))
                        }
                        tokenizer::TemplateKind::NoSub => {
                            let s = &s[1..s.len() - 1];
                            Token::Template(Template::NoSub(s))
                        }
                    }
                }
            };
            self.bump_line_cursors(new_lines, len);
            Item::new_(
                token,
                next.start,
                next.end,
                prev_lines.saturating_add(1),
                prev_line_cursor,
                self.new_line_count.saturating_add(1),
                self.line_cursor,
            )
        };

        if !advance_cursor {
            self.stream.stream.idx = prev_cursor;
            self.new_line_count = prev_lines;
            self.line_cursor = prev_line_cursor;
        } else {
            if let Token::Punct(ref p) = &ret.token {
                if let Punct::OpenParen = p {
                    ::std::mem::replace(
                        &mut self.before_last_open,
                        ::std::mem::replace(&mut self.last_three, LookBehind::new()),
                    );
                }
            }
            if !next.ty.is_comment() {
                self.last_three.push((&next.ty).into());
            }
        }
        let (new_line_count, leading_whitespace) = self.stream.skip_whitespace();
        self.bump_line_cursors(new_line_count, leading_whitespace);
        self.pending_new_line = new_line_count > 0;
        Some(Ok(ret))
    }

    /// Get a string for any given span
    pub fn string_for(&self, span: &Span) -> Option<String> {
        Some(self.str_for(span)?.to_string())
    }
    /// Get a &str for any given span
    pub fn str_for(&self, span: &Span) -> Option<&'b str> {
        if self.original.len() < span.start || self.original.len() < span.end {
            None
        } else {
            Some(&self.original[span.start..span.end])
        }
    }
    /// Get the line/column pair for any given byte index
    pub fn position_for(&self, idx: usize) -> (usize, usize) {
        let mut line_ct = 1;
        // This is the byte position, not the character
        // position to account for multi byte chars
        let mut byte_position = 0;
        // loop over the characters
        for (i, c) in self.original.chars().enumerate() {
            if i >= idx {
                return (line_ct, byte_position);
            }
            match c {
                '\r' => {
                    // look ahead 1 char to see if it is a newline pair
                    // if so, don't include it, it will get included in the next
                    // iteration
                    if let Some(next) = self.original.get(byte_position..byte_position + 2) {
                        if next != "\r\n" {
                            line_ct += 1;
                            byte_position = 0;
                        }
                    }
                }
                '\n' | '\u{2028}' | '\u{2029}' => {
                    line_ct += 1;
                    byte_position = 0;
                }
                _ => byte_position += c.len_utf8(),
            };
        }
        (line_ct, byte_position)
    }
    #[inline]
    /// Helper to handle new lines
    fn bump_line_cursors(&mut self, ct: usize, len: usize) {
        if ct != 0 {
            self.line_cursor = len;
            self.new_line_count += ct;
        } else {
            self.line_cursor += len;
        }
    }
    /// Helper to handle the error cases
    fn error<T>(&self, raw_error: RawError) -> Res<T> {
        let RawError { idx, msg } = raw_error;
        let (line, column) = self.position_for(idx);
        Err(Error { line, column, msg })
    }
}

#[derive(Clone, Copy, PartialEq, Debug)]
/// For keeping track of the nested-ness of
/// templates and blocks
pub enum OpenCurlyKind {
    Template,
    Block,
}

#[derive(Clone)]
/// All of the important state
/// for the scanner, used to
/// cache and reset a `Scanner`
pub struct ScannerState {
    pub cursor: usize,
    pub curly_stack: Vec<OpenCurlyKind>,
    pub new_line_count: usize,
    pub line_cursor: usize,
    pub last_three: LookBehind,
    pub paren_three: LookBehind,
}

#[cfg(test)]
mod test {
    use super::tokens::*;
    use super::*;
    #[test]
    fn tokenizer() {
        let js = "function thing(): void {
    let x = 0;
    console.log('stuff');
}";
        let expectation = vec![
            Token::Keyword(Keyword::Fn),
            Token::Ident("thing".into()),
            Token::Punct(Punct::OpenParen),
            Token::Punct(Punct::CloseParen),
            Token::Punct(Punct::Colon),
            Token::Keyword(Keyword::Void),
            Token::Punct(Punct::OpenBrace),
            Token::Keyword(Keyword::Let),
            Token::Ident("x".into()),
            Token::Punct(Punct::Equal),
            Token::Number("0".into()),
            Token::Punct(Punct::SemiColon),
            Token::Ident("console".into()),
            Token::Punct(Punct::Period),
            Token::Ident("log".into()),
            Token::Punct(Punct::OpenParen),
            Token::String(StringLit::Single("stuff")),
            Token::Punct(Punct::CloseParen),
            Token::Punct(Punct::SemiColon),
            Token::Punct(Punct::CloseBrace),
            Token::EoF,
        ];
        for (lhs, rhs) in Scanner::new(js).zip(expectation.into_iter()) {
            let lhs = lhs.unwrap();
            assert_eq!(lhs.token, rhs);
        }
    }

    #[test]
    fn tok_scanner() {
        let s = super::Scanner::new(
            "(function() {
this.x = 100;
this.y = 0;
})();",
        );
        let expected = vec![
            Token::Punct(Punct::OpenParen), //"("
            Token::Keyword(Keyword::Fn),
            Token::Punct(Punct::OpenParen),  //"("
            Token::Punct(Punct::CloseParen), //")"
            Token::Punct(Punct::OpenBrace),  //"{"
            Token::Keyword(Keyword::This),
            Token::Punct(Punct::Period), //"."
            Token::Ident("x".into()),
            Token::Punct(Punct::Equal), //"="
            Token::Number("100".into()),
            Token::Punct(Punct::SemiColon), //";"
            Token::Keyword(Keyword::This),
            Token::Punct(Punct::Period), //"."
            Token::Ident("y".into()),
            Token::Punct(Punct::Equal), //"="
            Token::Number("0".into()),
            Token::Punct(Punct::SemiColon),  //";"
            Token::Punct(Punct::CloseBrace), //"}"
            Token::Punct(Punct::CloseParen), //")"
            Token::Punct(Punct::OpenParen),  //"("
            Token::Punct(Punct::CloseParen), //")"
            Token::Punct(Punct::SemiColon),  //";"
            Token::EoF,
        ];
        validate(s, expected);
    }

    #[test]
    fn look_ahead() {
        let js = "function() { return; }";
        let mut s = Scanner::new(js);
        while let Some(peek) = s.look_ahead() {
            let peek = peek.unwrap();
            if let Some(next) = s.next() {
                let next = next.unwrap();
                assert_eq!(peek, next);
            }
        }
    }

    fn validate(s: Scanner, expected: Vec<Token<&str>>) {
        for (i, (lhs, rhs)) in s.zip(expected.into_iter()).enumerate() {
            let lhs = lhs.unwrap();
            println!("{:?}, {:?}", lhs.token, rhs);
            assert_eq!((i, lhs.token), (i, rhs));
        }
    }

    #[test]
    fn get_str() {
        let js = "function ( ) { return ; }";
        let mut s = Scanner::new(js);
        let strs = js.split(' ');
        for (i, p) in strs.enumerate() {
            let item = s.next().unwrap().unwrap();
            let q = s.string_for(&item.span).unwrap();
            assert_eq!((i, p.to_string()), (i, q))
        }
    }

    #[test]
    fn error() {
        let js = "
(function() {
    let x = 'asdf
    ';
})()";
        for item in Scanner::new(js) {
            match item {
                Ok(_) => (),
                Err(e) => {
                    assert_eq!(e.line, 3);
                    assert_eq!(e.column, 17);
                }
            }
        }
    }

    #[test]
    fn locations() {
        let js = r"(function() {
    let x = 'asdf\
';
    let y = `asd
f`;
    /*
    * things
    */
})();";
        let expectation = vec![
            SourceLocation::new(Position::new(1, 1), Position::new(1, 2)), // 0 (
            SourceLocation::new(Position::new(1, 2), Position::new(1, 10)), // 1 function
            SourceLocation::new(Position::new(1, 10), Position::new(1, 11)), // 2 (
            SourceLocation::new(Position::new(1, 11), Position::new(1, 12)), // 3 )
            SourceLocation::new(Position::new(1, 13), Position::new(1, 14)), // 4 {
            SourceLocation::new(Position::new(2, 5), Position::new(2, 8)), // 5 let
            SourceLocation::new(Position::new(2, 9), Position::new(2, 10)), // 6 x
            SourceLocation::new(Position::new(2, 11), Position::new(2, 12)), // 7 =
            SourceLocation::new(Position::new(2, 13), Position::new(3, 1)), // 8 'asdf'
            SourceLocation::new(Position::new(3, 1), Position::new(3, 2)), // 9 ;
            SourceLocation::new(Position::new(4, 5), Position::new(4, 8)), // 10 let
            SourceLocation::new(Position::new(4, 9), Position::new(4, 10)), // 11 y
            SourceLocation::new(Position::new(4, 11), Position::new(4, 12)), // 12 =
            SourceLocation::new(Position::new(4, 13), Position::new(5, 2)), // 13 `asdf`
            SourceLocation::new(Position::new(5, 2), Position::new(5, 3)), // 14 ;
            SourceLocation::new(Position::new(6, 5), Position::new(8, 6)), // 15 comment
            SourceLocation::new(Position::new(9, 1), Position::new(9, 2)), // 16 }
            SourceLocation::new(Position::new(9, 2), Position::new(9, 3)), // 17 )
            SourceLocation::new(Position::new(9, 3), Position::new(9, 4)), // 18 (
            SourceLocation::new(Position::new(9, 4), Position::new(9, 5)), // 19 )
            SourceLocation::new(Position::new(9, 5), Position::new(9, 6)), // 20 ;
        ];
        for (i, (lhs, rhs)) in Scanner::new(js).zip(expectation.iter()).enumerate() {
            let item = lhs.expect("error parsing item");
            assert_eq!((i, item.location), (i, *rhs))
        }
    }
}
