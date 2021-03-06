use wa1_lexer::prelude::*;
use wa1_ast::prelude::*;
use wa1_types::prelude::*;
pub use wa1_errs::Error;
pub use wa1_errs::prelude::*;

use crate::Parser;
use crate::ParserContext;
use crate::{create_cast, cast_typed_expr};

use lazy_static;

lazy_static!{
    static ref COMPARISON_OP: OpType = OpType::SimpleOpType(vec![
        FuncType{in_types: vec![FullType::new_const(&INT_S_64), FullType::new_const(&INT_S_64)], out_type: FullType::new_const(&Type::Bool)},
        FuncType{in_types: vec![FullType::new_const(&INT_S_32), FullType::new_const(&INT_S_32)], out_type: FullType::new_const(&Type::Bool)},
        FuncType{in_types: vec![FullType::new_const(&INT_U_64), FullType::new_const(&INT_U_64)], out_type: FullType::new_const(&Type::Bool)},
        FuncType{in_types: vec![FullType::new_const(&INT_U_32), FullType::new_const(&INT_U_32)], out_type: FullType::new_const(&Type::Bool)},
        FuncType{in_types: vec![FullType::new_const(&Type::Number), FullType::new_const(&Type::Number)], out_type: FullType::new_const(&Type::Bool)},
        FuncType{in_types: vec![FullType::new_const(&Type::UnsafePtr), FullType::new_const(&Type::UnsafePtr)], out_type: FullType::new_const(&Type::Bool)},
    ]);
    
    static ref MATHS_BIN_OP: OpType = OpType::SimpleOpType(vec![
        FuncType{in_types: vec![FullType::new_const(&INT_S_64), FullType::new_const(&INT_S_64)], out_type: FullType::new_const(&INT_S_64)},
        FuncType{in_types: vec![FullType::new_const(&INT_S_32), FullType::new_const(&INT_S_32)], out_type: FullType::new_const(&INT_S_32)},
        FuncType{in_types: vec![FullType::new_const(&INT_U_64), FullType::new_const(&INT_U_64)], out_type: FullType::new_const(&INT_U_64)},
        FuncType{in_types: vec![FullType::new_const(&INT_U_32), FullType::new_const(&INT_U_32)], out_type: FullType::new_const(&INT_U_32)},
        FuncType{in_types: vec![FullType::new_const(&Type::Number), FullType::new_const(&Type::Number)], out_type: FullType::new_const(&Type::Number)},
        FuncType{in_types: vec![FullType::new_const(&Type::UnsafePtr), FullType::new_const(&Type::UnsafePtr)], out_type: FullType::new_const(&Type::UnsafePtr)},
    ]);
    
    static ref MATHS_UN_OP: OpType = OpType::SimpleOpType(vec![
        FuncType{in_types: vec![FullType::new_const(&INT_S_64)], out_type: FullType::new_const(&INT_S_64)},
        FuncType{in_types: vec![FullType::new_const(&INT_U_64)], out_type: FullType::new_const(&INT_U_64)},
        FuncType{in_types: vec![FullType::new_const(&INT_S_32)], out_type: FullType::new_const(&INT_S_32)},
        FuncType{in_types: vec![FullType::new_const(&INT_U_32)], out_type: FullType::new_const(&INT_U_32)},
        FuncType{in_types: vec![FullType::new_const(&Type::Number)], out_type: FullType::new_const(&Type::Number)},
        FuncType{in_types: vec![FullType::new_const(&Type::UnsafePtr)], out_type: FullType::new_const(&Type::UnsafePtr)},
    ]);

    static ref MATHS_NEG_OP: OpType = OpType::SimpleOpType(vec![
        FuncType{in_types: vec![FullType::new_const(&INT_S_64)], out_type: FullType::new_const(&INT_S_64)},
        FuncType{in_types: vec![FullType::new_const(&INT_S_32)], out_type: FullType::new_const(&INT_S_32)},
        FuncType{in_types: vec![FullType::new_const(&Type::Number)], out_type: FullType::new_const(&Type::Number)},
        FuncType{in_types: vec![FullType::new_const(&Type::UnsafePtr)], out_type: FullType::new_const(&Type::UnsafePtr)},
    ]);
    
    static ref BIT_BIN_OP: OpType = OpType::SimpleOpType(vec![
        FuncType{in_types: vec![FullType::new_const(&INT_S_64), FullType::new_const(&INT_S_64)], out_type: FullType::new_const(&INT_S_64)},
        FuncType{in_types: vec![FullType::new_const(&INT_U_64), FullType::new_const(&INT_U_64)], out_type: FullType::new_const(&INT_U_64)},
        FuncType{in_types: vec![FullType::new_const(&INT_S_32), FullType::new_const(&INT_S_32)], out_type: FullType::new_const(&INT_S_32)},
        FuncType{in_types: vec![FullType::new_const(&INT_U_32), FullType::new_const(&INT_U_32)], out_type: FullType::new_const(&INT_U_32)},
        FuncType{in_types: vec![FullType::new_const(&Type::UnsafePtr), FullType::new_const(&Type::UnsafePtr)], out_type: FullType::new_const(&Type::UnsafePtr)},
    ]);
    
    static ref EQUALITY_OP: OpType = OpType::EqualityOpType;
    
    static ref AS_OP: OpType = OpType::AsOpType;

    static ref DOT_OP: OpType = OpType::StaticMemberOpType;
    
    static ref BOOL_BIN_OP: OpType = OpType::SimpleOpType(vec![
        FuncType{in_types: vec![FullType::new_const(&Type::Bool), FullType::new_const(&Type::Bool)], out_type: FullType::new_const(&Type::Bool)},
    ]);
    
    static ref BOOL_UN_OP: OpType = OpType::SimpleOpType(vec![
        FuncType{in_types: vec![FullType::new_const(&Type::Bool)], out_type: FullType::new_const(&Type::Bool)},
    ]);
    
    static ref BIT_UN_OP: OpType = OpType::SimpleOpType(vec![
        FuncType{in_types: vec![FullType::new_const(&INT_S_64)], out_type: FullType::new_const(&INT_S_64)},
        FuncType{in_types: vec![FullType::new_const(&INT_U_64)], out_type: FullType::new_const(&INT_U_64)},
        FuncType{in_types: vec![FullType::new_const(&INT_S_32)], out_type: FullType::new_const(&INT_S_32)},
        FuncType{in_types: vec![FullType::new_const(&INT_U_32)], out_type: FullType::new_const(&INT_U_32)},
    ]);
    
    static ref MATHS_ASSIGN_MODIFY_OP: OpType = OpType::AssignModifyOpType(vec![
        INT_S_32,
        INT_U_32,
        INT_S_64,
        INT_U_64,
        Type::Number,
        Type::UnsafePtr,
    ]);
    
    static ref BIT_ASSIGN_MODIFY_OP: OpType = OpType::AssignModifyOpType(vec![
        INT_S_32,
        INT_U_32,
        INT_S_64,
        INT_U_64,
    ]);
    
    /// clearly it would be nice to get these types
    static ref NO_IDEA_OP: OpType = OpType::NotImplementedOpType;
}

fn get_op_type_for_binop(binop: &BinaryOperator) -> &OpType {
    match binop {
        BinaryOperator::Dot => &DOT_OP,
        BinaryOperator::GreaterThan => &COMPARISON_OP,
        BinaryOperator::LessThan => &COMPARISON_OP,
        BinaryOperator::Plus => &MATHS_BIN_OP,
        BinaryOperator::Minus => &MATHS_BIN_OP,
        BinaryOperator::Multiply => &MATHS_BIN_OP,
        BinaryOperator::Mod => &MATHS_BIN_OP,
        BinaryOperator::BitOr => &BIT_BIN_OP,
        BinaryOperator::BitAnd => &BIT_BIN_OP,
        BinaryOperator::BitXor => &BIT_BIN_OP,
        BinaryOperator::Divide => &MATHS_BIN_OP,
        BinaryOperator::StrictEqual => &EQUALITY_OP,
        BinaryOperator::StrictNotEqual => &EQUALITY_OP,
        BinaryOperator::LogicalAnd => &BOOL_BIN_OP,
        BinaryOperator::LogicalOr => &BOOL_BIN_OP,
        BinaryOperator::Equal => &EQUALITY_OP,
        BinaryOperator::NotEqual => &EQUALITY_OP,
        BinaryOperator::GreaterThanEqual => &COMPARISON_OP,
        BinaryOperator::LessThanEqual => &COMPARISON_OP,
        BinaryOperator::Exponent => &MATHS_BIN_OP,
        BinaryOperator::In => &NO_IDEA_OP,
        BinaryOperator::InstanceOf => &NO_IDEA_OP,
        BinaryOperator::As => &AS_OP,
    }
}

pub fn get_op_type_for_unop(unop: &UnaryOperator) -> &OpType {
    match unop {
        UnaryOperator::LogicalNot => &BOOL_UN_OP,
        UnaryOperator::BitNot => &BIT_UN_OP,
        UnaryOperator::PostfixIncrement => &MATHS_UN_OP,
        UnaryOperator::PostfixDecrement=> &MATHS_UN_OP,
        UnaryOperator::Minus => &MATHS_NEG_OP,
        UnaryOperator::Plus => &MATHS_UN_OP,
        UnaryOperator::PrefixIncrement => &MATHS_UN_OP,
        UnaryOperator::PrefixDecrement => &MATHS_UN_OP,
    }
}

fn get_op_type_for_assop(assop: &AssignmentOperator) -> &OpType {
    match assop {
        AssignmentOperator::Assign => &OpType::AssignmentOpType,
        AssignmentOperator::PlusAssign => &MATHS_ASSIGN_MODIFY_OP,
        AssignmentOperator::MinusAssign => &MATHS_ASSIGN_MODIFY_OP,
        AssignmentOperator::ExponentAssign => &MATHS_ASSIGN_MODIFY_OP,
        AssignmentOperator::MultiplyAssign => &MATHS_ASSIGN_MODIFY_OP,
        AssignmentOperator::DivideAssign => &MATHS_ASSIGN_MODIFY_OP,
        AssignmentOperator::ModAssign => &MATHS_ASSIGN_MODIFY_OP,
        AssignmentOperator::BitAndAssign => &BIT_ASSIGN_MODIFY_OP,
        AssignmentOperator::BitXorAssign => &BIT_ASSIGN_MODIFY_OP,
        AssignmentOperator::BitOrAssign => &BIT_ASSIGN_MODIFY_OP
    }
}
    
#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Association{
    Left, 
    Right,
}

pub struct BinaryOperatorData{
    pub precedence: i32,
    pub association: Association,
    pub is_member: bool,
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Fix{
    Prefix, 
    Postfix,
    Both
}

pub struct UnaryOperatorData{
    pub fix: Fix,
}

fn get_binary_operator_data_for_keyword(keyword: &Keyword) -> Option<BinaryOperatorData> {
    match keyword {
        Keyword::In => Some(BinaryOperatorData{precedence: 12, association: Association::Left, is_member: false}),
        Keyword::InstanceOf => Some(BinaryOperatorData{precedence: 12, association: Association::Left, is_member: false}),
        Keyword::As => Some(BinaryOperatorData{precedence: 12, association: Association::Left, is_member: false}),
        _ => None
    }
}

/// at the point where this could be a binary operator, what is its precedence and association
fn get_binary_operator_data_for_punct(punct: &Punct) -> Option<BinaryOperatorData> {
    match punct {
        Punct::Period => Some(BinaryOperatorData{precedence: 18, association: Association::Left, is_member: true}),
        Punct::Tilde => None,
        Punct::GreaterThan => Some(BinaryOperatorData{precedence: 11, association: Association::Left, is_member: false}),
        Punct::LessThan => Some(BinaryOperatorData{precedence: 11, association: Association::Left, is_member: false}),
        Punct::Equal => Some(BinaryOperatorData{precedence: 3, association: Association::Right, is_member: false}),
        Punct::Plus => Some(BinaryOperatorData{precedence: 13, association: Association::Left, is_member: false}),
        Punct::Dash => Some(BinaryOperatorData{precedence: 13, association: Association::Left, is_member: false}),
        Punct::Asterisk => Some(BinaryOperatorData{precedence: 14, association: Association::Left, is_member: false}),
        Punct::Percent => Some(BinaryOperatorData{precedence: 14, association: Association::Left, is_member: false}),
        Punct::Pipe => Some(BinaryOperatorData{precedence: 7, association: Association::Left, is_member: false}),
        Punct::Ampersand => Some(BinaryOperatorData{precedence: 9, association: Association::Left, is_member: false}),
        Punct::Caret => Some(BinaryOperatorData{precedence: 8, association: Association::Left, is_member: false}),
        Punct::ForwardSlash => Some(BinaryOperatorData{precedence: 14, association: Association::Left, is_member: false}),
        Punct::Ellipsis => None,
        Punct::TripleEqual => Some(BinaryOperatorData{precedence: 10, association: Association::Left, is_member: false}),
        Punct::BangDoubleEqual => Some(BinaryOperatorData{precedence: 10, association: Association::Left, is_member: false}),
        Punct::DoubleAsteriskEqual => Some(BinaryOperatorData{precedence: 3, association: Association::Right, is_member: false}),
        Punct::DoubleAmpersand => Some(BinaryOperatorData{precedence: 6, association: Association::Left, is_member: false}),
        Punct::DoublePipe => Some(BinaryOperatorData{precedence: 5, association: Association::Left, is_member: false}),
        Punct::DoubleEqual => Some(BinaryOperatorData{precedence: 10, association: Association::Left, is_member: false}),
        Punct::BangEqual => Some(BinaryOperatorData{precedence: 10, association: Association::Left, is_member: false}),
        Punct::PlusEqual => Some(BinaryOperatorData{precedence: 3, association: Association::Right, is_member: false}),
        Punct::DashEqual => Some(BinaryOperatorData{precedence: 3, association: Association::Right, is_member: false}),
        Punct::AsteriskEqual => Some(BinaryOperatorData{precedence: 3, association: Association::Right, is_member: false}),
        Punct::ForwardSlashEqual => Some(BinaryOperatorData{precedence: 3, association: Association::Right, is_member: false}),
        Punct::AmpersandEqual => Some(BinaryOperatorData{precedence: 3, association: Association::Right, is_member: false}),
        Punct::PipeEqual => Some(BinaryOperatorData{precedence: 3, association: Association::Right, is_member: false}),
        Punct::CaretEqual => Some(BinaryOperatorData{precedence: 3, association: Association::Right, is_member: false}),
        Punct::PercentEqual => Some(BinaryOperatorData{precedence: 3, association: Association::Right, is_member: false}),
        Punct::GreaterThanEqual => Some(BinaryOperatorData{precedence: 11, association: Association::Left, is_member: false}),
        Punct::LessThanEqual => Some(BinaryOperatorData{precedence: 11, association: Association::Left, is_member: false}),
        Punct::DoubleAsterisk => Some(BinaryOperatorData{precedence: 16, association: Association::Right, is_member: false}),
        _ => None
    }
}

pub fn get_binary_operator_data<'a>(token: &Token<&'a str>) -> Option<BinaryOperatorData> {
    match token {
        Token::Punct(p) => get_binary_operator_data_for_punct(p),
        Token::Keyword(p) => get_binary_operator_data_for_keyword(p),
        _ => None
    }
}

fn get_unary_operator_data_for_punct(punct: &Punct) -> Option<UnaryOperatorData> {
    match punct {
        Punct::Bang => Some(UnaryOperatorData{fix: Fix::Prefix}),
        Punct::Dash => Some(UnaryOperatorData{fix: Fix::Prefix}),
        Punct::DoubleDash => Some(UnaryOperatorData{fix: Fix::Both}),
        Punct::DoublePlus => Some(UnaryOperatorData{fix: Fix::Both}),
        Punct::Plus => Some(UnaryOperatorData{fix: Fix::Prefix}),
        Punct::Tilde => Some(UnaryOperatorData{fix: Fix::Prefix}),
        _ => None,
    }
}

pub fn get_unary_operator_data<'a>(token: &Token<&'a str>) -> Option<UnaryOperatorData> {
    match token {
        Token::Punct(p) => get_unary_operator_data_for_punct(p),
        _ => None
    }
}

fn as_l_value(
    typed_expr: &TypedExpr,
    parser_context: &ParserContext,
) -> Option<TypedLValueExpr> {
    match &typed_expr.expr {
        Expr::GlobalVariableUse(name) => { 
            let o_g = parser_context.get_global_var(&name);
            match o_g{
                Some(g) => {
                    if g.mutability == VariableMutability::Variable {
                        Some(TypedLValueExpr{r#type: typed_expr.r#type.clone(), expr: LValueExpr::GlobalVariableAssign(name.clone()), loc: typed_expr.loc})
                    } else {
                        None
                    }

                },
                None => None
            }
        },
        Expr::LocalVariableUse(name) => {
            let (_, sv) = parser_context.find_named_scoped_var_given_internal_name(&name);
            if sv.is_var() {
                Some(TypedLValueExpr{r#type: typed_expr.r#type.clone(), expr: LValueExpr::LocalVariableAssign(name.clone()), loc: typed_expr.loc})
            } else {
                None
            }
        },
        Expr::ClosureVariableUse(name) => {
            if typed_expr.type_is_const() {
                None
            } else {
                Some(TypedLValueExpr{r#type: typed_expr.r#type.clone(), expr: LValueExpr::ClosureVariableAssign(name.clone()), loc: typed_expr.loc})
            }
        },
        Expr::NamedMember(lhs, name) => {
            if typed_expr.type_is_const() {
                None
            } else {
                Some(TypedLValueExpr{r#type: typed_expr.r#type.clone(), expr: LValueExpr::StaticNamedMemberAssign(lhs.clone(), lhs.r#type.r#type.clone(), name.clone()), loc: typed_expr.loc})
            }
        },
        Expr::DynamicMember(outer, inner) => {
            if typed_expr.type_is_const() {
                None
            } else {
                Some(TypedLValueExpr{r#type: typed_expr.r#type.clone(), expr: LValueExpr::DynamicMemberAssign(outer.clone(), outer.r#type.r#type.clone(), inner.clone()), loc: typed_expr.loc})
            }
        },
        Expr::Parens(inner) => as_l_value(&inner, parser_context),
        Expr::FreeDowncast(inner) => as_l_value(&inner, parser_context),
        _ => None,
    }
}

impl<'b> Parser<'b> {
    fn get_binary_operator_for_token(&self, token: &Token<&'b str>) -> Option<BinaryOperator> {
        match token {
            Token::Keyword(ref k) => match k {
                Keyword::In => Some(BinaryOperator::In),
                Keyword::InstanceOf => Some(BinaryOperator::InstanceOf),
                Keyword::As => Some(BinaryOperator::As),
                _ => None
            },
            Token::Punct(ref p) => match p {
                Punct::Ampersand => Some(BinaryOperator::BitAnd),
                Punct::Asterisk => Some(BinaryOperator::Multiply),
                Punct::Period => Some(BinaryOperator::Dot),
                Punct::GreaterThan => Some(BinaryOperator::GreaterThan),
                Punct::LessThan => Some(BinaryOperator::LessThan),
                Punct::Plus => Some(BinaryOperator::Plus),
                Punct::Dash => Some(BinaryOperator::Minus),
                Punct::Percent => Some(BinaryOperator::Mod),
                Punct::Pipe => Some(BinaryOperator::BitOr),
                Punct::Caret => Some(BinaryOperator::BitXor),
                Punct::ForwardSlash => Some(BinaryOperator::Divide),
                Punct::TripleEqual => Some(BinaryOperator::StrictEqual),
                Punct::BangDoubleEqual => Some(BinaryOperator::StrictNotEqual),
                Punct::DoubleAmpersand => Some(BinaryOperator::LogicalAnd),
                Punct::DoublePipe => Some(BinaryOperator::LogicalOr),
                Punct::DoubleEqual => Some(BinaryOperator::Equal),
                Punct::BangEqual => Some(BinaryOperator::NotEqual),
                Punct::GreaterThanEqual => Some(BinaryOperator::GreaterThanEqual),
                Punct::LessThanEqual => Some(BinaryOperator::LessThanEqual),
                Punct::DoubleAsterisk => Some(BinaryOperator::Exponent),
                _ => None
            },
            _ => None
        }
    }

    fn get_assignment_operator_for_token(&self, token: &Token<&'b str>) -> Option<AssignmentOperator> {
        match token {
            Token::Punct(ref p) => match p {
                Punct::AmpersandEqual => Some(AssignmentOperator::BitAndAssign),
                Punct::AsteriskEqual => Some(AssignmentOperator::MultiplyAssign),
                Punct::CaretEqual => Some(AssignmentOperator::BitXorAssign),
                Punct::DashEqual => Some(AssignmentOperator::MinusAssign),
                Punct::DoubleAsteriskEqual => Some(AssignmentOperator::ExponentAssign),
                Punct::ForwardSlashEqual => Some(AssignmentOperator::DivideAssign),
                Punct::PercentEqual => Some(AssignmentOperator::ModAssign),
                Punct::PipeEqual => Some(AssignmentOperator::BitOrAssign),
                Punct::PlusEqual => Some(AssignmentOperator::PlusAssign),
                Punct::Equal => Some(AssignmentOperator::Assign),
                _ => None
            },
            _ => None
        }
    }

    /// Parse a binary operator.
    pub (crate) fn parse_binary_op(&mut self,
        op: &Token<&str>,
        lhs: &TypedExpr,
        rhs: &TypedExpr,
        parser_context: &mut ParserContext,
    ) -> TypedExpr {
        let loc = SourceLocation::new(lhs.loc.start.clone(), rhs.loc.end.clone());

        //first see if it's a simple binary operator
        let o_bin_op = self.get_binary_operator_for_token(&op);
        match o_bin_op {
            Some(bin_op) => {
                //type check the binary operator
                let op_type = get_op_type_for_binop(&bin_op);
                match *op_type {
                    OpType::AsOpType => {
                        //as is just a straight cast; so 
                        match &rhs.r#type.r#type {
                            Type::TypeLiteral(t) => {
                                cast_typed_expr(&t, Box::new(lhs.clone()), CastType::Explicit, parser_context)
                            },
                            _ => {
                                parser_context.errors.push(Error::AsNeedsType(loc));
                                lhs.clone()
                            }
                        }
                    },
                    OpType::StaticMemberOpType => {
                        //this is a member. Member operators rhs subsume the lhs, so we just let it go.
                        rhs.clone()
                    },
                    _ => {
                        let o_bin_op_type_cast = wa1_types::get_binary_op_type_cast(op_type, &lhs.r#type, &rhs.r#type);
                        match o_bin_op_type_cast {
                            Some(bin_op_type_cast) => {
                                let o_cast = create_cast(&bin_op_type_cast.lhs_type, lhs, &bin_op_type_cast.lhs_type_cast);
                                let lhs_out = match o_cast {
                                    Some(cast) => Box::new(cast),
                                    None => {
                                        parser_context.push_err(Error::TypeFailureBinaryOperator(loc, op.to_string(), lhs.r#type.clone(), rhs.r#type.clone()));
                                        Box::new(lhs.clone())
                                    }
                                };
    
                                let o_cast = create_cast(&bin_op_type_cast.rhs_type, rhs, &bin_op_type_cast.rhs_type_cast);
                                let rhs_out = match o_cast {
                                    Some(cast) => Box::new(cast),
                                    None => {
                                        parser_context.push_err(Error::TypeFailureBinaryOperator(loc, op.to_string(), lhs.r#type.clone(), rhs.r#type.clone()));
                                        Box::new(rhs.clone())
                                    }
                                };
                                
                                TypedExpr{expr: Expr::BinaryOperator{op: bin_op, lhs: lhs_out, rhs: rhs_out}, r#type: bin_op_type_cast.out_type, loc: loc, return_expr: ReturnExpr::None}
                            },
                            None => {
                                parser_context.push_err(Error::TypeFailureBinaryOperator(loc, op.to_string(), lhs.r#type.clone(), rhs.r#type.clone()));
                                TypedExpr{expr: Expr::BinaryOperator{op: bin_op, lhs: Box::new(lhs.clone()), rhs: Box::new(rhs.clone())}, r#type: FullType::new_const(&Type::Unknown), loc: loc, return_expr: ReturnExpr::None}
                            }
                        }
                    }
                } 
            },

            //it's not a regular binary op; so let's see if it's an assignment operator
            None => {
                let o_ass_op = self.get_assignment_operator_for_token(&op);
                match o_ass_op {
                    Some(ass_op) => {
                        //get an l-value. This will do const checking
                        let o_l_value = as_l_value(&lhs, parser_context);
                        match o_l_value {
                            None => {
                                parser_context.push_err(Error::NotAnLValue(loc));
                                lhs.clone()
                            },
                            Some(l_value) => {
                                //if we got something, we need to erase the typeguard
                                match &l_value.expr {
                                    LValueExpr::LocalVariableAssign(name) => {
                                        parser_context.unguard_var(name);
                                    },
                                    _ => {}
                                }

                                //The l-value code will have stripped away any type guards, so we use that
                                let unguarded_lhs_full_type = l_value.r#type.clone();

                                //now type check the assignment operator. Note that we only need a type check from the rhs
                                let o_bin_op_type_cast = wa1_types::get_binary_op_type_cast(get_op_type_for_assop(&ass_op), &unguarded_lhs_full_type, &rhs.r#type);
                                match o_bin_op_type_cast {
                                    Some(bin_op_type_cast) => {
                                        //drop any temporaries, because we are assigning this to a non temporary
                                        let rhs_no_temp: &TypedExpr = match &rhs.expr {
                                            Expr::TemporaryCreation(expr, temporary_name) => {
                                                parser_context.forget_temporary(&temporary_name);
                                                expr
                                            },
                                            _ => rhs
                                        };
                                        let o_cast = create_cast(&bin_op_type_cast.rhs_type, rhs_no_temp, &bin_op_type_cast.rhs_type_cast);
                                        let rhs_out = match o_cast {
                                            Some(cast) => Box::new(cast),
                                            None => {
                                                parser_context.push_err(Error::TypeFailureBinaryOperator(loc, op.to_string(), unguarded_lhs_full_type, rhs.r#type.clone()));
                                                Box::new(rhs.clone())
                                            }
                                        };
                                        if ass_op == AssignmentOperator::Assign {
                                            //if we don't know the assignment model, worry about it later
                                            if rhs_out.r#type.r#type.is_type_variable() {
                                                TypedExpr{expr: Expr::UnresolvedAssignment{l_value, r_value: rhs_out}, r#type: bin_op_type_cast.out_type, loc: loc, return_expr: ReturnExpr::None}
                                            } else {
                                                let is_standard_value_model = rhs_out.r#type.r#type.is_standard_value_model();
                                                if (is_standard_value_model && l_value.r#type.mutability == Mutability::Const && rhs_out.r#type.mutability == Mutability::Const) || !is_standard_value_model{
                                                    TypedExpr{expr: Expr::SimpleAssignment{l_value, r_value: rhs_out}, r#type: bin_op_type_cast.out_type, loc: loc, return_expr: ReturnExpr::None}
                                                } else {
                                                    //this needs to be a deep copy
                                                    unimplemented!()
                                                }
                                            }
                                        } else {
                                            TypedExpr{expr: Expr::ModifyAssignment(ass_op, Box::new(lhs.clone()), l_value, rhs_out), r#type: bin_op_type_cast.out_type, loc: loc, return_expr: ReturnExpr::None}
                                        } 
                                    },
                                    None => {
                                        parser_context.push_err(Error::TypeFailureBinaryOperator(loc, op.to_string(), unguarded_lhs_full_type.clone(), rhs.r#type.clone()));
                                        TypedExpr{expr: Expr::UnresolvedAssignment{l_value, r_value: Box::new(rhs.clone())}, r#type: FullType::new_const(&Type::Unknown), loc: loc, return_expr: ReturnExpr::None}
                                    }
                                }
                            }
                        }
                    }, 
                    None => {
                        parser_context.errors.push(Error::NotYetImplemented(loc, format!("{:#?}", op)));
                        lhs.clone()
                    }
                }
            }
        }
    }
}