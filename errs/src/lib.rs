use ress::{Position, SourceLocation};
use std::fmt::{Display, Formatter, Result};
use types::Type;

#[derive(Debug, Clone)]
pub enum Error {
    UnexpectedToken(SourceLocation, String),
    UnexpectedEoF(String),
    ParseAfterEoF,
    InvalidTypeName(SourceLocation, String),
    InvalidType(Position),
    Other(String),
    NotYetImplemented(SourceLocation, String),
    VariableNotRecognised(String),
    FuncNotRecognised(String),
    TypeFailureUnaryOperator,
    TypeFailureBinaryOperator(SourceLocation, String, String),
    TypeFailureVariableCreation(SourceLocation),
    TypeFailure(SourceLocation, Type, Type),
    NoValueReturned,
    TypeFailureReturn(Type, Type),
    NotAnLValue,
    ConstFailure,
    TypeFailureFuncCall,
    TooManyArgs,
    NotEnoughArgs,
    DuplicateTypeName(String),
    NotInLoop(String),
    TypeFailureIf(Type, Type),
    WhileMayNotReturn,
}

impl Display for Error {
    fn fmt(&self, f: &mut Formatter) -> Result {
        match self {
            Error::UnexpectedToken(ref loc, ref msg) => write!(f, "ERROR {}: unexpected token {}", loc, msg),
            Error::UnexpectedEoF(ref msg) => write!(f, "Unexpectedly found the end of the file: {}", msg),
            Error::ParseAfterEoF => write!(f, "Parser attempted to get the next token after finding the end of the file"),
            Error::InvalidTypeName(ref pos, ref msg) => write!(f, "ERROR {}: Invalid type name {}", pos, msg),
            Error::InvalidType(ref pos) => write!(f, "Invalid type, {}", pos),
            Error::Other(ref msg) => write!(f, "{}", msg),
            Error::NotYetImplemented(ref pos, ref msg) => write!(f, "ERROR {}: not yet implemented: {}", pos, msg),
            Error::VariableNotRecognised(ref var_name) => write!(f, "Var not recognised: {}", var_name),
            Error::FuncNotRecognised(ref func_name) => write!(f, "Func not recognised: {}", func_name),
            Error::TypeFailureUnaryOperator => write!(f, "Type failure unary operator"),
            Error::TypeFailureBinaryOperator(ref pos, ref lhs_type, ref rhs_type) => 
                write!(f, "ERROR {}: can't make type of lhs ({}) and rhs ({}) of binary operator agree", pos, lhs_type, rhs_type),
            Error::TypeFailureVariableCreation(ref pos) => write!(f, "ERROR {}: initialiser type doesn't match variable type", pos),
            Error::TypeFailure(ref pos, ref wanted, ref got) => write!(f, "ERROR {}: expecting expression of type {}, found {}", pos, wanted, got),
            Error::NoValueReturned => write!(f, "Must return a value"),
            Error::TypeFailureReturn(ref wanted, ref got) => write!(f, "Expecting return value of type {}, found {}", wanted, got),
            Error::NotAnLValue => write!(f, "Expression is not a l value"),
            Error::ConstFailure => write!(f, "Expression is const and may not be assigned to"),
            Error::TypeFailureFuncCall => write!(f, "Variable is not a function"),
            Error::TooManyArgs => write!(f, "Too many args"),
            Error::NotEnoughArgs => write!(f, "Not enough args"),
            Error::DuplicateTypeName(name) => write!(f, "Duplicate type name: {}", name),
            Error::NotInLoop(what) => write!(f, "Used loop keyword outside a loop: {}", what),
            Error::TypeFailureIf(then_type, else_type) => write!(f, "Branches of if staement to not match: {}, {}", then_type, else_type),
            Error::WhileMayNotReturn => write!(f, "while loops may not have a return value"),
        }
    }
}

impl Error {
}

pub fn pretty_print_errs(errs: &Vec<Error>) -> () {
    for err in errs {
        println!("{}", err);    
    }
}