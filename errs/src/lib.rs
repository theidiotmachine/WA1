use ress::Position;
use std::fmt::{Display, Formatter, Result};
#[derive(Debug, Clone)]
pub enum Error {
    UnexpectedToken(Position, String),
    UnexpectedEoF(String),
    ParseAfterEoF,
    InvalidTypeName(Position, String),
    InvalidType(Position),
    Other(String),
    NotYetImplemented(String),
    VariableNotRecognised(String),
}

impl Display for Error {
    fn fmt(&self, f: &mut Formatter) -> Result {
        match self {
            Error::UnexpectedToken(ref pos, ref msg) => write!(f, "Unexpected Token at {}{}: {}", pos.line, pos.column, msg),
            Error::UnexpectedEoF(ref msg) => write!(f, "Unexpectedly found the end of the file: {}", msg),
            Error::ParseAfterEoF => write!(f, "Parser attempted to get the next token after finding the end of the file"),
            Error::InvalidTypeName(ref pos, ref msg) => write!(f, "Invalid type name, {}: {}", pos, msg),
            Error::InvalidType(ref pos) => write!(f, "Invalid type, {}", pos),
            Error::Other(ref msg) => write!(f, "{}", msg),
            Error::NotYetImplemented(ref msg) => write!(f, "Not yet implemented: {}", msg),
            Error::VariableNotRecognised(ref var_name) => write!(f, "Var not recognised: {}", var_name),
        }
    }
}

impl Error {
}