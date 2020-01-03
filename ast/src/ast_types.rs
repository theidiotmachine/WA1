use types::*;
pub mod prelude {
    pub use super::UserType;
}

#[derive(Debug, Clone, PartialEq)]
pub enum UserType{
    Struct{struct_type: StructType, under_construction: bool},
    Class(ClassType)
}