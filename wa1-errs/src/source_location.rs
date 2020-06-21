use serde::{Serialize, Deserialize};

pub mod prelude {
    pub use super::Position;
    pub use super::SourceLocation;
}

#[derive(Clone, Copy, Debug, PartialEq, Serialize, Deserialize)]
/// A single character position in the
/// file including the line/column number
pub struct Position {
    pub line: usize,
    pub column: usize,
}

impl ::std::fmt::Display for Position {
    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        write!(f, "{}:{}", self.line, self.column)
    }
}

impl Position {
    #[inline]
    pub const fn new(line: usize, column: usize) -> Self {
        Self { line, column }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Serialize, Deserialize)]
/// The start and end position of a token
/// including the line/column number
pub struct SourceLocation {
    pub start: Position,
    pub end: Position,
}

impl SourceLocation {
    #[inline]
    pub const fn new(start: Position, end: Position) -> Self {
        Self { start, end }
    }

    pub fn extend_right(&mut self, right: &SourceLocation) {
        self.end = right.end.clone();
    }
}

impl ::std::fmt::Display for SourceLocation {
    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        write!(f, "{}-{}", self.start, self.end)
    }
}