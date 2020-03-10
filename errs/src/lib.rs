use std::fmt::{Display, Formatter, Result};
use types::Type;
pub mod source_location;
pub mod prelude {
    pub use super::source_location::prelude::*;
    pub use super::ErrRecorder;
}
use source_location::{SourceLocation, Position};

#[derive(Debug, Clone)]
pub enum Error {
    UnexpectedToken(SourceLocation, String),
    UnexpectedEoF(String),
    ParseAfterEoF,
    InvalidTypeName(SourceLocation, String),
    InvalidType(Position),
    Other(String),
    NotYetImplemented(SourceLocation, String),
    VariableNotRecognized(SourceLocation, String),
    FuncNotRecognized(SourceLocation, String),
    TypeFailureUnaryOperator(SourceLocation),
    TypeFailureBinaryOperator(SourceLocation, String, String),
    TypeFailureVariableCreation(SourceLocation, String, String),
    TypeFailureMemberCreation(SourceLocation, String, Type, Type),
    TypeFailure(SourceLocation, Type, Type),
    CastFailure(SourceLocation, Type, Type),
    NoValueReturned(SourceLocation),
    TypeFailureReturn(SourceLocation, Type, Type),
    NotAnLValue(SourceLocation),
    ConstFailure,
    TypeFailureFuncCall(SourceLocation),
    TooManyArgs(SourceLocation),
    NotEnoughArgs(SourceLocation),
    DuplicateTypeName(String),
    NotInLoop(String),
    TypeFailureIf(SourceLocation, Type, Type),
    WhileMayNotReturn,
    NoComponents(SourceLocation),
    CantConstructUsingObjectLiteral(SourceLocation, Type),
    CantConstructUsingArrayLiteral(SourceLocation, Type),
    ObjectHasNoMember(SourceLocation, Type, String,),
    ObjectDuplicateMember(SourceLocation, String),
    ObjectMissingMember(SourceLocation, String),
    AsNeedsType(SourceLocation),
    RecursiveTypeDefinition(SourceLocation),
    UnsafeCodeNotAllowed(SourceLocation),
    Dummy(SourceLocation),
    CompileFailed(String),
    ImportFailed(SourceLocation, String),
    UnrecognizedTypeArg(SourceLocation),
    UnrecognizedTypeArgConstraint(SourceLocation),
    NoClosureInGenerics(SourceLocation),
    MissingTypeArgs(SourceLocation),
    FailedTypeArgConstraint(SourceLocation),
    UnresolvedTypeArg(SourceLocation, String),
    DuplicateGlobalVariable(SourceLocation, String),
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
            Error::VariableNotRecognized(ref loc, ref var_name) => write!(f, "ERROR {}: identifier not recognized: {}", loc, var_name),
            Error::FuncNotRecognized(ref loc, ref func_name) => write!(f, "ERROR {}: function not recognized: {}", loc, func_name),
            Error::TypeFailureUnaryOperator(ref loc) => write!(f, "ERROR {}: Type failure unary operator", loc),
            Error::TypeFailureBinaryOperator(ref loc, ref lhs_type, ref rhs_type) => 
                write!(f, "ERROR {}: can't make type of lhs ({}) and rhs ({}) of binary operator agree", loc, lhs_type, rhs_type),
            Error::TypeFailureVariableCreation(ref loc, ref wanted, ref got) => write!(f, "ERROR {}: initializer type {} doesn't match variable type {}", loc, got, wanted),
            Error::TypeFailureMemberCreation(ref loc, ref m, ref wanted, ref got) => write!(f, "ERROR {}: initializer of type {} of member {} doesn't match member type {}", loc, got, m, wanted),
            Error::TypeFailure(ref loc, ref wanted, ref got) => write!(f, "ERROR {}: expecting expression of type {}, found {}", loc, wanted, got),
            Error::CastFailure(ref loc, ref wanted, ref got) => write!(f, "ERROR {}: can't cast to type {}, from {}", loc, wanted, got),
            Error::NoValueReturned(ref loc) => write!(f, "ERROR {}: must return a value", loc),
            Error::TypeFailureReturn(ref loc, ref wanted, ref got) => write!(f, "ERROR {}: Expecting return value of type {}, found {}", loc, wanted, got),
            Error::NotAnLValue(ref loc) => write!(f, "ERROR {}: expression is not a l value", loc),
            Error::ConstFailure => write!(f, "Expression is const and may not be assigned to"),
            Error::TypeFailureFuncCall(ref loc) => write!(f, "ERROR: {}: Variable is not a function", loc),
            Error::TooManyArgs(ref loc) => write!(f, "ERROR {}: too many args for function call", loc),
            Error::NotEnoughArgs(ref loc) => write!(f, "ERROR {}: not enough args for function call", loc),
            Error::DuplicateTypeName(name) => write!(f, "Duplicate type name: {}", name),
            Error::NotInLoop(what) => write!(f, "Used loop keyword outside a loop: {}", what),
            Error::TypeFailureIf(ref loc, ref then_type, ref else_type) => write!(f, "WARNING {}: Types of branches of if statement do not match; then branch is of type {}, else branch is of type {}", loc, then_type, else_type),
            Error::WhileMayNotReturn => write!(f, "while loops may not have a return value"),
            Error::NoComponents(ref loc) => write!(f, "ERROR {}: object has no components", loc),
            Error::CantConstructUsingObjectLiteral(ref loc, ref t) => write!(f, "ERROR {}: can't construct type {} from an object literal", loc, t),
            Error::CantConstructUsingArrayLiteral(ref loc, ref t) => write!(f, "ERROR {}: can't construct type {} from an array literal", loc, t),
            Error::ObjectHasNoMember(ref loc, ref t, ref m) => write!(f, "ERROR {}: object of type {} has no member {}", loc, t, m),
            Error::ObjectDuplicateMember(ref loc, ref m) => write!(f, "ERROR {}: object has multiple definitions for {}", loc, m),
            Error::ObjectMissingMember(ref loc, ref m) => write!(f, "ERROR {}: object requires member {}", loc, m),
            Error::AsNeedsType(ref loc) => write!(f, "ERROR {}: 'as' must have a type literal on its rhs", loc),
            Error::RecursiveTypeDefinition(ref loc) => write!(f, "ERROR {}: recursive type definition not allowed", loc),
            Error::UnsafeCodeNotAllowed(ref loc) => write!(f, "ERROR {}: unsafe code not allowed to be called without --unsafe", loc),
            Error::Dummy(ref loc) => write!(f, "ERROR {}: internal error", loc),
            Error::CompileFailed(ref err) => write!(f, "ERROR: could not compile because {}", err),
            Error::ImportFailed(ref loc, ref mes) => write!(f, "ERROR {}: import failed because {}", loc, mes),
            Error::UnrecognizedTypeArg(ref loc) => write!(f, "ERROR {}: can't parse type arg", loc),
            Error::UnrecognizedTypeArgConstraint(ref loc) => write!(f, "ERROR {}: unrecognized type arg constraint", loc),
            Error::FailedTypeArgConstraint(ref loc) => write!(f, "ERROR {}: failed type arg constraint", loc),
            Error::NoClosureInGenerics(ref loc) => write!(f, "ERROR {}: generic functions may not capture closures", loc),
            Error::MissingTypeArgs(ref loc) => write!(f, "ERROR {}: not all type arguments supplied", loc),
            Error::UnresolvedTypeArg(ref loc, ref name) => write!(f, "INTERNAL ERROR {}: unresolved type arg {}", loc, name),
            Error::DuplicateGlobalVariable(ref loc, ref name) => write!(f, "ERROR {}: duplicate global variable declaration {}", loc, name),
        }
    }
}

impl Error {
    pub fn is_warning(&self) -> bool {
        match self {
            Error::TypeFailureIf(_, _, _) => true,
            _ => false
        }
    }
}

pub fn pretty_print_errs(errs: &Vec<Error>) -> () {
    for err in errs {
        println!("{}", err);    
    }
}

pub trait ErrRecorder{
    fn push_err(&mut self, err: Error) -> ();
}