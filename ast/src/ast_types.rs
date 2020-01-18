use serde::{Serialize, Deserialize};

use types::*;
pub mod prelude {
    pub use super::TypeDecl;
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum TypeDecl{
    Struct{name: String, struct_type: StructType, under_construction: bool, export: bool},
    Class{name: String, class_type: ClassType, export: bool},
}