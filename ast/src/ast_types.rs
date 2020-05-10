use serde::{Serialize, Deserialize};

use types::*;
use types::prelude::TypeArg;
pub mod prelude {
    pub use super::{TypeDecl, MemberFunc};
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct MemberFunc{
    pub type_args: Vec<TypeArg>,
    pub func_type: FuncType,
    pub mangled_name: String,
    pub name: String,
    pub privacy: Privacy,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum TypeDecl{
    Struct{name: String, struct_type: StructType, under_construction: bool, export: bool},
    Alias{name: String, of: Type, export: bool},
    Type{name: String, inner: Type, type_args: Vec<TypeArg>, export: bool, member_funcs: Vec<MemberFunc>, constructor: Option<MemberFunc>, under_construction: bool},
}