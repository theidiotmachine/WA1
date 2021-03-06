use serde::{Serialize, Deserialize};

use wa1_types::*;
use wa1_types::prelude::TypeArg;
pub mod prelude {
    pub use super::{TypeDecl, MemberFunc, TraitDecl, TraitMemberFunc, TraitImpl, UserClassStorage};
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

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, Copy)]
pub enum UserClassStorage{
    Heap,
    Stack
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum TypeDecl{
    Struct{members: Vec<Member>, under_construction: bool, export: bool},
    Alias{of: Type, export: bool},
    Type{inner: Type, type_args: Vec<TypeArg>, export: bool, member_funcs: Vec<MemberFunc>, constructor: Option<MemberFunc>, under_construction: bool},
    UserClass{type_args: Vec<TypeArg>, export: bool, member_funcs: Vec<MemberFunc>, constructor: Option<MemberFunc>, under_construction: bool, members: Vec<Member>, storage: UserClassStorage}
}

impl TypeDecl{
    pub fn get_member_funcs(&self) -> Vec<MemberFunc> {
        match self {
            TypeDecl::Struct{members: _, under_construction: _, export: _} | TypeDecl::Alias{of: _, export: _} => vec![],
            TypeDecl::Type{inner: _, type_args: _, export: _, member_funcs, constructor: _, under_construction: _} 
                | TypeDecl::UserClass{type_args: _, export: _, member_funcs, constructor: _, under_construction: _, members: _, storage: _} => member_funcs.clone()
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