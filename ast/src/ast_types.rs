use serde::{Serialize, Deserialize};

use types::*;
use types::prelude::TypeArg;
pub mod prelude {
    pub use super::{TypeDecl, MemberFunc, TraitDecl, TraitMemberFunc, TraitImpl};
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct MemberFunc{
    pub type_args: Vec<TypeArg>,
    pub func_type: FuncType,
    ///This is the full name with the type prefix in front of it.
    pub mangled_name: String,
    ///This is the name you would type.
    pub name: String,
    pub privacy: Privacy,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct TraitMemberFunc{
    pub trait_name: String,
    pub type_args: Vec<TypeArg>,
    pub func_type: FuncType,
    pub name: String,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum TypeDecl{
    Struct{name: String, struct_type: StructType, under_construction: bool, export: bool},
    Alias{name: String, of: Type, export: bool},
    Type{name: String, inner: Type, type_args: Vec<TypeArg>, export: bool, member_funcs: Vec<MemberFunc>, constructor: Option<MemberFunc>, under_construction: bool},
}

impl TypeDecl{
    pub fn get_member_funcs(&self) -> Vec<MemberFunc> {
        match self {
            TypeDecl::Struct{name: _, struct_type: _, under_construction: _, export: _} | TypeDecl::Alias{name: _, of: _, export: _} => vec![],
            TypeDecl::Type{name: _, inner: _, type_args: _, export: _, member_funcs, constructor: _, under_construction: _} => member_funcs.clone()
        }
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct TraitDecl{
    pub name: String, 
    pub member_funcs: Vec<TraitMemberFunc>, 
    pub export: bool,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct TraitImpl{
    pub trait_name: String, 
    pub for_type: Type,
    pub export: bool,
    pub member_funcs: Vec<MemberFunc>, 
}