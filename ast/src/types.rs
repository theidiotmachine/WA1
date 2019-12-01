pub mod prelude {
    pub use super::Type;
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    /// unit type
    Void,
    /// top type
    Unknown,
    /// bottom type
    Never,

    ///number
    Number,
    ///string
    String,
    ///Array
    Array(Box<Type>),
    ///64 bit int
    BigInt,


    ///Tuple
    Tuple(Vec<Type>),
    
    ///object
    Object,

    ///literal
    //Literal,

    /// boxed type
    Any,

    /// user type
    User(String),

    /// not yet known - will be filled in by the typer
    Undeclared
    
}
