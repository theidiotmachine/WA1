use serde::{Serialize, Deserialize};
/*
/// This is part of the type system rewrite. I am not sure it's correct yet.
/// This is the body of a type function that consumes type args and returns a type.
#[derive(Debug, Clone, PartialEq)]
pub enum AbstractTypeBody{
    VariableUse(String),
    Array(Box<AbstractTypeBody>),
    Func(Vec<AbstractTypeBody>, Box<AbstractTypeBody>),
    UserType,
    Number,
    String,
    Boolean,
    Any,
    Unknown, //???
    RealVoid,
    FakeVoid,
    Never,
}

/// This is part of the type system rewrite. I am not sure it's correct yet.
/// This is a type function that consumes type args and returns a type.
#[derive(Debug, Clone, PartialEq)]
pub struct AbstractTypeDecl{
    pub args: Vec<String>,
    pub out: AbstractTypeBody,
}
*/

#[derive(Clone, PartialEq, Debug, Deserialize, Serialize)]
pub enum TypeArgConstraint{
    None,
    IsAStruct,
}

#[derive(Clone, PartialEq, Debug, Deserialize, Serialize)]
pub struct TypeArg{
    pub name: String,
    pub constraint: TypeArgConstraint,
}
