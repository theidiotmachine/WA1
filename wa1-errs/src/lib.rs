use std::fmt::{Display, Formatter, Result};
use wa1_types::{Type, FullType};
pub mod source_location;
pub mod prelude {
    pub use super::source_location::prelude::*;
    pub use super::ErrRecorder;
}
use source_location::{SourceLocation};

#[derive(Debug, Clone)]
pub enum Error {
    UnexpectedToken(SourceLocation, String),
    UnexpectedEoF(SourceLocation, String),
    ParseAfterEoF,
    InvalidTypeName(SourceLocation, String),
    InvalidType(SourceLocation),
    Other(String),
    NotYetImplemented(SourceLocation, String),
    VariableNotRecognized(SourceLocation, String),
    FuncNotRecognized(SourceLocation, String),
    TypeFailureUnaryOperator(SourceLocation),
    TypeFailureBinaryOperator(SourceLocation, String, FullType, FullType),
    TypeFailureVariableCreation(SourceLocation, String, String),
    TypeFailureMemberCreation(SourceLocation, String, Type, Type),
    TypeFailure(SourceLocation, FullType, FullType),
    CastFailure(SourceLocation, FullType, FullType),
    ConstFailure(SourceLocation, FullType, FullType),
    CastNotNeeded(SourceLocation, Type),
    NoValueReturned(SourceLocation),
    TypeFailureReturn(SourceLocation, Type, Type),
    NotAnLValue(SourceLocation),
    TypeFailureFuncCall(SourceLocation),
    TooManyArgs(SourceLocation),
    NotEnoughArgs(SourceLocation),
    DuplicateTypeName(SourceLocation, String),
    NotInLoop(SourceLocation, String),
    NoComponents(SourceLocation, Type),
    CantConstruct(SourceLocation, Type),
    CantConstructUsingObjectLiteral(SourceLocation, Type),
    CantConstructUsingArrayLiteral(SourceLocation, Type),
    ObjectHasNoMember(SourceLocation, Type, String,),
    ObjectDuplicateMember(SourceLocation, String),
    ObjectMissingMember(SourceLocation, String),
    AsNeedsType(SourceLocation),
    RecursiveTypeDefinition(SourceLocation),
    UnsafeCodeNotAllowed(SourceLocation),
    InternalError(SourceLocation, String),
    ImportFailed(SourceLocation, String),
    UnrecognizedTypeArg(SourceLocation),
    UnrecognizedTypeArgConstraint(SourceLocation),
    NoClosureInGenerics(SourceLocation),
    MissingTypeArgs(SourceLocation),
    FailedTypeArgConstraint(SourceLocation),
    UnresolvedTypeArg(SourceLocation, String),
    DuplicateGlobalVariable(SourceLocation, String),
    TypeGuardExpectingLiteral(SourceLocation),
    FailedGenericDeduction(SourceLocation, String, Type),
    ExportedFunctionFatArrow(SourceLocation),
    IntegerOutOfRange(SourceLocation, i128, i128),
    TypeGuardReapply(SourceLocation, Type),
    NoThis(SourceLocation),
    OnlyOneConstructor(SourceLocation),
    EncapsulationFailure(SourceLocation, Type, String),
    UnnecessaryMut(SourceLocation, Type),
    CantImplementTraitFor(SourceLocation, Type),
    WrongNumberOfTypeArgs(SourceLocation),
    UnrecognizedTrait(SourceLocation, String),
    MemberNotImplemented(SourceLocation, String),
}

impl Display for Error {
    fn fmt(&self, f: &mut Formatter) -> Result {
        match self {
            Error::UnexpectedToken(ref loc, ref msg) => write!(f, "ERROR {}: unexpected token {}", loc, msg),
            Error::UnexpectedEoF(ref loc, ref msg) => write!(f, "ERROR {}: unexpectedly found the end of the file: {}", loc, msg),
            Error::ParseAfterEoF => write!(f, "Parser attempted to get the next token after finding the end of the file"),
            Error::InvalidTypeName(ref loc, ref msg) => write!(f, "ERROR {}: Invalid type name {}", loc, msg),
            Error::InvalidType(ref loc) => write!(f, "ERROR {}: Invalid type", loc),
            Error::Other(ref msg) => write!(f, "{}", msg),
            Error::NotYetImplemented(ref loc, ref msg) => write!(f, "ERROR {}: not yet implemented: {}", loc, msg),
            Error::VariableNotRecognized(ref loc, ref var_name) => write!(f, "ERROR {}: identifier not recognized: {}", loc, var_name),
            Error::FuncNotRecognized(ref loc, ref func_name) => write!(f, "ERROR {}: function not recognized: {}", loc, func_name),
            Error::TypeFailureUnaryOperator(ref loc) => write!(f, "ERROR {}: Type failure unary operator", loc),
            Error::TypeFailureBinaryOperator(ref loc, ref op, ref lhs_type, ref rhs_type) => 
                write!(f, "ERROR {}: can't make type of lhs ({}) and rhs ({}) of binary operator {} agree", loc, lhs_type, rhs_type, op),
            Error::TypeFailureVariableCreation(ref loc, ref wanted, ref got) => write!(f, "ERROR {}: initializer type {} doesn't match variable type {}", loc, got, wanted),
            Error::TypeFailureMemberCreation(ref loc, ref m, ref wanted, ref got) => write!(f, "ERROR {}: initializer of type {} of member {} doesn't match member type {}", loc, got, m, wanted),
            Error::TypeFailure(ref loc, ref wanted, ref got) => write!(f, "ERROR {}: expecting expression of type {}, found {}", loc, wanted, got),
            Error::CastFailure(ref loc, ref wanted, ref got) => write!(f, "ERROR {}: can't cast to type {}, from {}", loc, wanted, got),
            Error::ConstFailure(ref loc, ref wanted, ref got) => write!(f, "ERROR {}: can't cast to type {}, from {}", loc, wanted, got),
            Error::CastNotNeeded(ref loc, ref wanted) => write!(f, "WARNING {}: no need to cast to type {}", loc, wanted),
            Error::NoValueReturned(ref loc) => write!(f, "ERROR {}: must return a value", loc),
            Error::TypeFailureReturn(ref loc, ref wanted, ref got) => write!(f, "ERROR {}: Expecting return value of type {}, found {}", loc, wanted, got),
            Error::NotAnLValue(ref loc) => write!(f, "ERROR {}: expression is not a l value", loc),
            Error::TypeFailureFuncCall(ref loc) => write!(f, "ERROR {}: Variable is not a function", loc),
            Error::TooManyArgs(ref loc) => write!(f, "ERROR {}: too many args for function call", loc),
            Error::NotEnoughArgs(ref loc) => write!(f, "ERROR {}: not enough args for function call", loc),
            Error::DuplicateTypeName(ref loc, name) => write!(f, "ERROR {}: Duplicate type name: {}", loc, name),
            Error::NotInLoop(ref loc, what) => write!(f, "ERROR {}: Used loop keyword outside a loop: {}", loc, what),
            Error::NoComponents(ref loc, ref t) => write!(f, "ERROR {}: object of type {} has no components", loc, t),
            Error::CantConstruct(ref loc, ref t) => write!(f, "ERROR {}: can't construct type {}", loc, t),
            Error::CantConstructUsingObjectLiteral(ref loc, ref t) => write!(f, "ERROR {}: can't construct type {} from an object literal", loc, t),
            Error::CantConstructUsingArrayLiteral(ref loc, ref t) => write!(f, "ERROR {}: can't construct type {} from an array literal", loc, t),
            Error::ObjectHasNoMember(ref loc, ref t, ref m) => write!(f, "ERROR {}: object of type {} has no member {}", loc, t, m),
            Error::ObjectDuplicateMember(ref loc, ref m) => write!(f, "ERROR {}: object has multiple definitions for {}", loc, m),
            Error::ObjectMissingMember(ref loc, ref m) => write!(f, "ERROR {}: object requires member {}", loc, m),
            Error::AsNeedsType(ref loc) => write!(f, "ERROR {}: 'as' must have a type literal on its rhs", loc),
            Error::RecursiveTypeDefinition(ref loc) => write!(f, "ERROR {}: recursive type definition not allowed", loc),
            Error::UnsafeCodeNotAllowed(ref loc) => write!(f, "ERROR {}: unsafe code not allowed to be called without --unsafe", loc),
            Error::InternalError(ref loc, ref m) => write!(f, "INTERNAL ERROR {}: {}", loc, m),
            Error::ImportFailed(ref loc, ref mes) => write!(f, "ERROR {}: import failed because {}", loc, mes),
            Error::UnrecognizedTypeArg(ref loc) => write!(f, "ERROR {}: can't parse type arg", loc),
            Error::UnrecognizedTypeArgConstraint(ref loc) => write!(f, "ERROR {}: unrecognized type arg constraint", loc),
            Error::FailedTypeArgConstraint(ref loc) => write!(f, "ERROR {}: failed type arg constraint", loc),
            Error::NoClosureInGenerics(ref loc) => write!(f, "ERROR {}: generic functions may not capture closures", loc),
            Error::MissingTypeArgs(ref loc) => write!(f, "ERROR {}: not all type arguments supplied", loc),
            Error::UnresolvedTypeArg(ref loc, ref name) => write!(f, "INTERNAL ERROR {}: unresolved type arg {}", loc, name),
            Error::DuplicateGlobalVariable(ref loc, ref name) => write!(f, "ERROR {}: duplicate global variable declaration {}", loc, name),
            Error::TypeGuardExpectingLiteral(ref loc) => write!(f, "ERROR {}: type guard must be a literal", loc),
            Error::FailedGenericDeduction(ref loc, ref name, ref t) => write!(f, "ERROR {}: unable to deduce type of type arg {}. Setting it to {}", loc, name, t),
            Error::ExportedFunctionFatArrow(ref loc) => write!(f, "ERROR {}: exported functions must have explicit return type", loc),
            Error::IntegerOutOfRange(ref loc, v, bound) => write!(f, "ERROR {}: integer out of bounds. Value {} does not conform to bound {}", loc, v, bound),
            Error::TypeGuardReapply(ref loc, ref t) => write!(f, "WARNING {}: typeguard {} is already in place", loc, t),
            Error::NoThis(ref loc) => write!(f, "ERROR {}: 'this' not valid in this context", loc),
            Error::OnlyOneConstructor(ref loc) => write!(f, "ERROR {}: only one constructor is supported", loc),
            Error::EncapsulationFailure(ref loc, ref t, ref m) => write!(f, "ERROR {}: not allowed to access member {} of {}", loc, m, t),
            Error::UnnecessaryMut(ref loc, ref t) => write!(f, "WARNING{}: type {} can't be mut, ignoring. Did you mean to mark the variable var?", loc, t),
            Error::CantImplementTraitFor(ref loc, ref t) => write!(f, "Error{}: type {} can't have a trait implemented for it", loc, t),
            Error::WrongNumberOfTypeArgs(ref loc) => write!(f, "Error{}: wrong number of type args", loc),
            Error::UnrecognizedTrait(ref loc, ref name) => write!(f, "ERROR {}: unrecognized trait {}", loc, name),
            Error::MemberNotImplemented(ref loc, ref name) => write!(f, "ERROR {}: member {} has not been implemented", loc, name),
        }
    }
}

impl Error {
    pub fn is_warning(&self) -> bool {
        match self {
            Error::TypeGuardReapply(_, _) | Error::CastNotNeeded(_, _) | Error::UnnecessaryMut(_, _) => true,
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