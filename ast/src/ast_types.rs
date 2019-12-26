use types::*;
pub mod prelude {
    pub use super::UserType;
}

#[derive(Debug, Clone, PartialEq)]
pub enum UserType{
    Struct(StructType),
    Class(ClassType)
}