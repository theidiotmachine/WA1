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
    VariableNotRecognised(SourceLocation, String),
    FuncNotRecognised(String),
    TypeFailureUnaryOperator,
    TypeFailureBinaryOperator(SourceLocation, String, String),
    TypeFailureVariableCreation(SourceLocation, String, String),
    TypeFailureMemberCreation(SourceLocation, String, String, String),
    TypeFailure(SourceLocation, Type, Type),
    NoValueReturned(SourceLocation),
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
    NoComponents(SourceLocation),
    CantConstructUsingObjectLiterall(SourceLocation, String),
    ObjectHasNoMember(SourceLocation, String),
    ObjectDuplicateMember(SourceLocation, String),
    ObjectMissingMember(SourceLocation, String),
}

impl Display for Error {
    fn fmt(&self, f: &mut Formatter) -> Result {
        match self {
            Error::UnexpectedToken(ref loc, ref msg) => write!(f, "ERROR {}: unexpected token {}", loc, msg),
            Error::UnexpectedEoF(ref msg) => write!(f, "Unexpectedly found the end of the file: {}", msg),
            Error::ParseAfterEoF => write!(f, "Parser attempted to get the next token after finding the end of the file"),
            Error::InvalidTypeName(ref loc, ref msg) => write!(f, "ERROR {}: Invalid type name {}", loc, msg),
            Error::InvalidType(ref loc) => write!(f, "Invalid type, {}", loc),
            Error::Other(ref msg) => write!(f, "{}", msg),
            Error::NotYetImplemented(ref loc, ref msg) => write!(f, "ERROR {}: not yet implemented: {}", loc, msg),
            Error::VariableNotRecognised(ref loc, ref var_name) => write!(f, "ERROR {}: var not recognised: {}", loc, var_name),
            Error::FuncNotRecognised(ref func_name) => write!(f, "Func not recognised: {}", func_name),
            Error::TypeFailureUnaryOperator => write!(f, "Type failure unary operator"),
            Error::TypeFailureBinaryOperator(ref loc, ref lhs_type, ref rhs_type) => 
                write!(f, "ERROR {}: can't make type of lhs ({}) and rhs ({}) of binary operator agree", loc, lhs_type, rhs_type),
            Error::TypeFailureVariableCreation(ref loc, ref wanted, ref got) => write!(f, "ERROR {}: initialiser type {} doesn't match variable type {}", loc, got, wanted),
            Error::TypeFailureMemberCreation(ref loc, ref m, ref wanted, ref got) => write!(f, "ERROR {}: initialiser type {} of {} doesn't match variable type {}", loc, got, m, wanted),
            Error::TypeFailure(ref loc, ref wanted, ref got) => write!(f, "ERROR {}: expecting expression of type {}, found {}", loc, wanted, got),
            Error::NoValueReturned(ref loc) => write!(f, "ERROR {}: must return a value", loc),
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
            Error::NoComponents(ref loc) => write!(f, "ERROR {}: object has no components", loc),
            Error::CantConstructUsingObjectLiterall(ref loc, ref t) => write!(f, "ERROR {}: can't construct type {} from an object literal", loc, t),
            Error::ObjectHasNoMember(ref loc, ref m) => write!(f, "ERROR {}: object has no member {}", loc, m),
            Error::ObjectDuplicateMember(ref loc, ref m) => write!(f, "ERROR {}: object has multiple definitions for {}", loc, m),
            Error::ObjectMissingMember(ref loc, ref m) => write!(f, "ERROR {}: object requires member {}", loc, m),
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