use std::cmp;

use crate::ParserContext;

use wa1_ast::prelude::*;
use wa1_types::prelude::*;
pub use wa1_errs::Error;
pub use wa1_errs::prelude::*;


fn try_get_local_variable(expr: &TypedExpr) -> Option<String> {
    match &expr.expr {
        Expr::LocalVariableUse(name) => {
            Some(name.clone())
        },
        Expr::FreeUpcast(inner) => try_get_local_variable(&inner),
        Expr::FreeDowncast(inner) => try_get_local_variable(&inner),
        Expr::FreeGenericCast(inner) => try_get_local_variable(&inner),
        _ => None
    }
}

/// Any ordering binary operator could have widened the underlying int to make it work;
/// for typeguard purposes we want to reach under that to get the actual type.
fn get_true_int_type(expr: &TypedExpr) -> Option<(i128, i128)> {
    match &expr.expr {
        Expr::IntWiden(x) => get_true_int_type(&*x),
        _ => {
            match expr.r#type.r#type {
                Type::Int(l, u) => Some((l, u)),
                _ => None
            }
        },
    }
}

/// See if we can turn an ordering binary operator into a typeguard.
fn get_type_guard_bounded_int_inst(lhs: &TypedExpr, rhs: &TypedExpr, op: BinaryOperator) -> Vec<(TypeGuard, String)> {
    let o_lhs_i = get_true_int_type(lhs);
    let (lhs_lower, lhs_upper) = if o_lhs_i.is_none() {
        return vec![]
    } else {
        o_lhs_i.unwrap()
    };

    let o_rhs_i = get_true_int_type(rhs);
    let (rhs_lower, rhs_upper) = if o_rhs_i.is_none() {
        return vec![]
    } else {
        o_rhs_i.unwrap()
    };

    let o_lhs_name = try_get_local_variable(lhs);
    let o_rhs_name = try_get_local_variable(rhs);
    if o_lhs_name.is_some() && o_rhs_name.is_some() || o_lhs_name.is_none() && o_rhs_name.is_none() {
        return vec![];
    }

    let (name, op, var_lower, var_upper, literal_lower, literal_upper) = if o_rhs_name.is_some() {
        let inv_op = match op{
            BinaryOperator::LessThan => BinaryOperator::GreaterThanEqual,
            BinaryOperator::LessThanEqual => BinaryOperator::GreaterThan,
            BinaryOperator::GreaterThan => BinaryOperator::LessThanEqual,
            BinaryOperator::GreaterThanEqual => BinaryOperator::LessThan,
            _ => unreachable!()
        };
        (o_rhs_name.unwrap(), inv_op, rhs_lower, rhs_upper, lhs_lower, lhs_upper)
    } else {
        (o_lhs_name.unwrap(), op, lhs_lower, lhs_upper, rhs_lower, rhs_upper)
    };

    let true_expr = TypedExpr{expr: Expr::BoolLiteral(true), r#type: FullType::new(&Type::Bool, Mutability::Const), loc: SourceLocation::new(Position::new(0, 0), Position::new(0, 0))};
    let false_expr = TypedExpr{expr: Expr::BoolLiteral(false), r#type: FullType::new(&Type::Bool, Mutability::Const), loc: SourceLocation::new(Position::new(0, 0), Position::new(0, 0))};
    let type_guard = match op {
        BinaryOperator::LessThan => TypeGuard{ branches: vec![
            TypeGuardBranch{literal: true_expr, guard_type: Type::Int(var_lower, cmp::min(var_upper, literal_upper - 1))},
            TypeGuardBranch{literal: false_expr, guard_type: Type::Int(cmp::max(var_lower, literal_lower), var_upper)}
        ] },
        BinaryOperator::LessThanEqual => TypeGuard{ branches: vec![
            TypeGuardBranch{literal: true_expr, guard_type: Type::Int(var_lower, cmp::min(var_upper, literal_upper))},
            TypeGuardBranch{literal: false_expr, guard_type: Type::Int(cmp::max(var_lower, literal_lower + 1), var_upper)}
        ] },
        BinaryOperator::GreaterThan => TypeGuard{ branches: vec![
            TypeGuardBranch{literal: true_expr, guard_type: Type::Int(cmp::max(var_lower, literal_lower + 1), var_upper)},
            TypeGuardBranch{literal: false_expr, guard_type: Type::Int(var_lower, cmp::min(var_upper, literal_upper))}
        ] },
        BinaryOperator::GreaterThanEqual => TypeGuard{ branches: vec![
            TypeGuardBranch{literal: true_expr, guard_type: Type::Int(cmp::max(var_lower, literal_lower), var_upper)},
            TypeGuardBranch{literal: false_expr, guard_type: Type::Int(var_lower, cmp::min(var_upper, literal_upper - 1))},
        ] },
        _ => unreachable!()
    };
    vec![(type_guard, name)]
}

/// See if this conditional is a type guard; and if it is, get it and the name of the variable it operates on
pub (crate) fn get_type_guard_inst(condition: &Expr) -> Vec<(TypeGuard, String)> {
    match condition {
        Expr::StaticFuncCall(_, fd, v) => {
            let o_type_guard = &fd.type_guard;
            if o_type_guard.is_some() && v.len() == 1 {
                let o_name = try_get_local_variable(&v[0]);
                match o_name{
                    Some(name) => vec![(o_type_guard.clone().unwrap(), name.clone())],
                    None => vec![],
                }
            } else {
                vec![]
            }
        },
        Expr::BinaryOperator{lhs, rhs, op} => {
            match op {
                BinaryOperator::LogicalAnd => {
                    let mut l = get_type_guard_inst(&lhs.expr);
                    let r = get_type_guard_inst(&rhs.expr);
                    l.extend(r);
                    l
                },
                BinaryOperator::LessThan | BinaryOperator::LessThanEqual | BinaryOperator::GreaterThan | BinaryOperator::GreaterThanEqual => {
                    get_type_guard_bounded_int_inst(&lhs, &rhs, *op)
                },
                _ => vec![]
            }
        },
        _ => vec![]
    }
}

/// If given an instance of a type guard, find the branch that corresponds to this type guard branch, 
/// and push a guarded variable into a new block. The return value tells you whether you need to pop.
pub (crate) fn apply_type_guard_inst(
    type_guard_insts: &Vec<(TypeGuard, String)>, 
    branch_expr: &Expr,
    loc: &SourceLocation,
    parser_context: &mut ParserContext
) -> bool {
    let mut pushed_scope = false;
    for (type_guard, variable_name) in type_guard_insts {
        let o_branch = type_guard.branches.iter().find(|b| b.literal.expr == *branch_expr);
        if o_branch.is_some() {
            let branch = o_branch.unwrap();
            if !pushed_scope {
                parser_context.push_block_scope();
                pushed_scope = true
            }
            
            parser_context.guard_var(&variable_name, &branch.guard_type, loc);
        }
    } 
    type_guard_insts.len() > 0
}

pub (crate) fn unapply_type_guard_inst(
    type_guard_insts: &Vec<(TypeGuard, String)>, 
    branch_expr: &Expr,
    parser_context: &mut ParserContext
) -> () {
    let mut pop_scope = false;
    for (type_guard, variable_name) in type_guard_insts {
        let o_branch = type_guard.branches.iter().find(|b| b.literal.expr == *branch_expr);
        if o_branch.is_some() {
            pop_scope = true;
        }

        parser_context.unguard_var(&variable_name);
    }
    if pop_scope {
        parser_context.pop_block_scope();
    }
}

