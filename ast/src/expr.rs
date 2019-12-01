pub mod prelude {
    pub use super::Expr;
    pub use super::BinaryOperator;
    pub use super::BinaryOperatorApplication;
    pub use super::UnaryOperator;
    pub use super::UnaryOperatorApplication;
}

#[derive(Debug, Clone, PartialEq)]
pub enum BinaryOperator{
    Dot,
    Comma,
    GreaterThan,
    LessThan,
    Plus,
    Minus,
    Multiply,
    Mod,
    BitOr,
    BitAnd,
    BitXor,
    Divide,
    StrictEqual,
    StrictNotEqual,
    UnsignedRightShift,
    LogicalAnd,
    LogicalOr,
    Equal,
    NotEqual,
    LeftShift,
    RightShift,
    GreaterThanEqual,
    LessThanEqual,
    Exponent,
    In,
    InstanceOf,
    //the assignment operators. These are considered ops in js, even though they sideeffect
    Assign,
    PlusAssign,
    MinusAssign,
    ExponentAssign,
    MultiplyAssign,
    DivideAssign,
    ModAssign,
    LeftShiftAssign,
    RightShiftAssign,
    UnsignedRightShiftAssign,
    BitAndAssign,
    BitXorAssign,
    BitOrAssign
}

impl BinaryOperator{
    pub fn is_assign_operator(&self) -> bool {
        match self {
            BinaryOperator::Assign => true,
            BinaryOperator::PlusAssign => true,
            BinaryOperator::MinusAssign => true,
            BinaryOperator::ExponentAssign => true,
            BinaryOperator::MultiplyAssign => true,
            BinaryOperator::DivideAssign => true,
            BinaryOperator::ModAssign => true,
            BinaryOperator::LeftShiftAssign => true,
            BinaryOperator::RightShiftAssign => true,
            BinaryOperator::UnsignedRightShiftAssign => true,
            BinaryOperator::BitAndAssign => true,
            BinaryOperator::BitXorAssign => true,
            BinaryOperator::BitOrAssign => true,
            _ => false
        }
    }

    pub fn is_simple_operator(&self) -> bool {
        match self {
            BinaryOperator::GreaterThan => true,
            BinaryOperator::LessThan => true,
            BinaryOperator::Plus => true,
            BinaryOperator::Minus => true,
            BinaryOperator::Multiply => true,
            BinaryOperator::Mod => true,
            BinaryOperator::BitOr => true,
            BinaryOperator::BitAnd => true,
            BinaryOperator::BitXor => true,
            BinaryOperator::Divide => true,
            BinaryOperator::StrictEqual => true,
            BinaryOperator::StrictNotEqual => true,
            BinaryOperator::UnsignedRightShift => true,
            BinaryOperator::LogicalAnd => true,
            BinaryOperator::LogicalOr => true,
            BinaryOperator::Equal => true,
            BinaryOperator::NotEqual => true,
            BinaryOperator::LeftShift => true,
            BinaryOperator::RightShift => true,
            BinaryOperator::GreaterThanEqual => true,
            BinaryOperator::LessThanEqual => true,
            BinaryOperator::Exponent => true,
            BinaryOperator::In => true,
            _ => false
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct BinaryOperatorApplication{
    pub lhs: Box<Expr>,
    pub rhs: Box<Expr>, 
    pub op: BinaryOperator,
}

#[derive(Debug, Clone, PartialEq)]
pub enum UnaryOperator{
    LogicalNot,
    BitNot,
    PostfixIncrement,
    PostfixDecrement,
    Minus,
    Plus,
    PrefixIncrement,
    PrefixDecrement,
}

#[derive(Debug, Clone, PartialEq)]
pub struct UnaryOperatorApplication {
    pub expr: Box<Expr>,
    pub op: UnaryOperator,
}

/// Something that returns something
#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    /// A binary operator
    BinaryOperator(BinaryOperatorApplication),
    /// unary operator
    UnaryOperator(UnaryOperatorApplication),
    /// float literal
    FloatLiteral(f64),
    /// int literal
    //IntLiteral(i64),
    ///true or false
    BoolLiteral(bool),
    //null
    Null,
    //void. Unlike TS we use void as the unit type, so you can instantiate it.
    Void,
    //string literal
    StringLiteral(String),
    //variable call
    GlobalVariableUse(String),
    LocalVariableUse(String),
    ClosureVariableUse(String),
    ParameterUse(String),
    //round brackets
    Parens(Box<Expr>),
}
