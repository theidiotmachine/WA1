use ress::Position;
use std::fmt::{Display, Formatter, Result};
use types::Type;

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
    TypeFailureUnaryOperator,
    TypeFailureBinaryOperator,
    TypeFailureVariableCreation,
    TypeFailure(Type, Type),
    NoValueReturned,
    TypeFailureReturn(Type, Type),
    NotAnLValue,
    ConstFailure,
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
            Error::TypeFailureUnaryOperator => write!(f, "Type failure unary operator"),
            Error::TypeFailureBinaryOperator => write!(f, "Type failure binary operator"),
            Error::TypeFailureVariableCreation => write!(f, "Type failure variable creation"),
            Error::TypeFailure(ref wanted, ref got) => write!(f, "Expecting expression of type {}, found {}", wanted, got),
            Error::NoValueReturned => write!(f, "Must return a value"),
            Error::TypeFailureReturn(ref wanted, ref got) => write!(f, "Expecting return value of type {}, found {}", wanted, got),
            Error::NotAnLValue => write!(f, "Expression is not a l value"),
            Error::ConstFailure => write!(f, "Expression is const and may not be assigned to"),
        }
    }
}

impl Error {
}