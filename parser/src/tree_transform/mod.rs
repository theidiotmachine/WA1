use ast::prelude::*;
pub use errs::Error;
pub use errs::prelude::*;

pub trait Transform{
    fn transform_typed_expr(&mut self, typed_expr: &TypedExpr, parser_context: &mut dyn ErrRecorder) -> Option<TypedExpr>;
    fn transform_expr(&mut self, expr: &Expr, loc: &SourceLocation, parser_context: &mut dyn ErrRecorder) -> Option<Expr>;
    fn transform_typed_lvalue_expr(&mut self, typed_lvalue_expr: &TypedLValueExpr, parser_context: &mut dyn ErrRecorder) -> Option<TypedLValueExpr>;
    fn transform_lvalue_expr(&mut self, lvalue_expr: &LValueExpr, loc: &SourceLocation, parser_context: &mut dyn ErrRecorder) -> Option<LValueExpr>;
}

pub fn transform_lvalue_expr(
    expr: &LValueExpr,
    transform: &mut dyn Transform,
    loc: &SourceLocation, 
    parser_context: &mut dyn ErrRecorder,
) -> LValueExpr {
    let first_pass = transform.transform_lvalue_expr(expr, loc, parser_context);
    match first_pass{
        Some(out) => return out,
        None => {}
    }
    match expr {
        LValueExpr::ClosureVariableAssign(_) | LValueExpr::GlobalVariableAssign(_) | LValueExpr::LocalVariableAssign(_) => expr.clone(),
        LValueExpr::StaticNamedMemberAssign(te, tle, s) => LValueExpr::StaticNamedMemberAssign(
            Box::new(transform_typed_expr(te, transform, parser_context)),
            Box::new(transform_typed_lvalue_expr(tle, transform, parser_context)),
            s.clone(),
        ),
        LValueExpr::DynamicMemberAssign(te1, tle, te2) => LValueExpr::DynamicMemberAssign(
            Box::new(transform_typed_expr(te1, transform, parser_context)),
            Box::new(transform_typed_lvalue_expr(tle, transform, parser_context)),
            Box::new(transform_typed_expr(te2, transform, parser_context)),
        )
    }
}

pub fn transform_typed_lvalue_expr(
    typed_lvalue_expr: &TypedLValueExpr,
    transform: &mut dyn Transform,
    parser_context: &mut dyn ErrRecorder,
) -> TypedLValueExpr {
    let first_pass = transform.transform_typed_lvalue_expr(typed_lvalue_expr, parser_context);
    match first_pass{
        Some(out) => return out,
        None => {}
    }

    TypedLValueExpr{
        expr: transform_lvalue_expr(&typed_lvalue_expr.expr, transform, &typed_lvalue_expr.loc, parser_context), 
        loc: typed_lvalue_expr.loc, r#type: typed_lvalue_expr.r#type.clone()
    }
}

fn transform_typed_exprs(
    v: &Vec<TypedExpr>,
    transform: &mut dyn Transform,
    parser_context: &mut dyn ErrRecorder,
) -> Vec<TypedExpr>{
    let mut out = vec![];
    for e in v {
        out.push(transform_typed_expr(e, transform, parser_context));
    }
    out
}

pub fn transform_expr(
    expr: &Expr,
    transform: &mut dyn Transform,
    loc: &SourceLocation, 
    parser_context: &mut dyn ErrRecorder,
) -> Expr {
    let first_pass = transform.transform_expr(expr, loc, parser_context);
    match first_pass{
        Some(out) => return out,
        None => {}
    }
    match expr {
        Expr::BigIntLiteral(_) | Expr::BoolLiteral(_) | Expr::Break | Expr::ClassDecl(_) | Expr::ClosureVariableUse(_) 
            | Expr::Continue | Expr::FloatLiteral(_) | Expr::GlobalVariableUse(_) | Expr::IntLiteral(_) 
            | Expr::LocalVariableUse(_) | Expr::NoOp | Expr::Null | Expr::StringLiteral(_) | Expr::StructDecl(_) 
            | Expr::UnsafeNull | Expr::Void
            => expr.clone(),

        Expr::Assignment(l1, l2, r) => {
            Expr::Assignment(
                Box::new(transform_typed_expr(l1, transform, parser_context)),
                transform_typed_lvalue_expr(l2, transform, parser_context),
                Box::new(transform_typed_expr(r, transform, parser_context)),
            )
        },
        
        Expr::BinaryOperator{lhs, rhs, op} => Expr::BinaryOperator{
            lhs: Box::new(transform_typed_expr(lhs, transform, parser_context)),
            rhs: Box::new(transform_typed_expr(rhs, transform, parser_context)),
            op: op.clone()
        },
        Expr::Block(v) => Expr::Block(transform_typed_exprs(v, transform, parser_context)),
        Expr::ConstructFromObjectLiteral(t, v) => {
            let mut oles = vec![];
            for ole in v {
                oles.push(ObjectLiteralElem{name: ole.name.clone(), value: transform_typed_expr(&ole.value, transform, parser_context)})
            }
            Expr::ConstructFromObjectLiteral(t.clone(), oles)
        },
        Expr::ConstructStaticFromArrayLiteral(t, v) => Expr::ConstructStaticFromArrayLiteral(t.clone(), transform_typed_exprs(v, transform, parser_context)),
        Expr::ConstructStaticFromObjectLiteral(t, v) => {
            let mut oles = vec![];
            for ole in v {
                oles.push(ObjectLiteralElem{name: ole.name.clone(), value: transform_typed_expr(&ole.value, transform, parser_context)})
            }
            Expr::ConstructStaticFromObjectLiteral(t.clone(), oles)
        },
        Expr::DynamicFuncCall(te, v) => 
            Expr::DynamicFuncCall(Box::new(transform_typed_expr(te, transform, parser_context)), transform_typed_exprs(v, transform, parser_context)),
        Expr::DynamicMember(te1, te2) =>
            Expr::DynamicMember(Box::new(transform_typed_expr(te1, transform, parser_context)), Box::new(transform_typed_expr(te2, transform, parser_context))),
        Expr::FreeTypeWiden(te) => Expr::FreeTypeWiden(Box::new(transform_typed_expr(te, transform, parser_context))),
        Expr::FuncDecl(foc) => Expr::FuncDecl(foc.clone()),
        Expr::GlobalVariableDecl(gvd) => 
            Expr::GlobalVariableDecl(Box::new(GlobalVariableDecl{name: gvd.name.clone(), r#type: gvd.r#type.clone(), constant: gvd.constant, export: gvd.export, 
                init: gvd.init.as_ref().map(|e| transform_typed_expr(&e, transform, parser_context))
            })),
        Expr::IfThen(te1, te2) =>
            Expr::IfThen(Box::new(transform_typed_expr(te1, transform, parser_context)), Box::new(transform_typed_expr(te2, transform, parser_context))),
        Expr::IfThenElse(te1, te2, te3) =>
            Expr::IfThenElse(
                Box::new(transform_typed_expr(te1, transform, parser_context)), 
                Box::new(transform_typed_expr(te2, transform, parser_context)),
                Box::new(transform_typed_expr(te3, transform, parser_context))
            ),
        Expr::Intrinsic(i) => {
            match i {
                Intrinsic::I32Clz(te) => Expr::Intrinsic(Intrinsic::I32Clz(Box::new(transform_typed_expr(te, transform, parser_context)))),
                Intrinsic::I32Ctz(te) => Expr::Intrinsic(Intrinsic::I32Ctz(Box::new(transform_typed_expr(te, transform, parser_context)))),
                Intrinsic::I32ShL(te1, te2) => Expr::Intrinsic(Intrinsic::I32ShL(Box::new(transform_typed_expr(te1, transform, parser_context)),
                    Box::new(transform_typed_expr(te2, transform, parser_context)))),
                Intrinsic::I32ShRS(te1, te2) => Expr::Intrinsic(Intrinsic::I32ShRS(Box::new(transform_typed_expr(te1, transform, parser_context)),
                    Box::new(transform_typed_expr(te2, transform, parser_context)))),
                Intrinsic::I32ShRU(te1, te2) => Expr::Intrinsic(Intrinsic::I32ShRU(Box::new(transform_typed_expr(te1, transform, parser_context)),
                    Box::new(transform_typed_expr(te2, transform, parser_context)))),
                Intrinsic::I64Ctz(te) => Expr::Intrinsic(Intrinsic::I64Ctz(Box::new(transform_typed_expr(te, transform, parser_context)))),
                Intrinsic::MemoryGrow(te) => Expr::Intrinsic(Intrinsic::MemoryGrow(Box::new(transform_typed_expr(te, transform, parser_context)))),
                Intrinsic::MemorySize => Expr::Intrinsic(Intrinsic::MemorySize),
                Intrinsic::Trap => Expr::Intrinsic(Intrinsic::Trap),
            }
        },
        Expr::IntToBigInt(te) => Expr::IntToBigInt(Box::new(transform_typed_expr(te, transform, parser_context))),
        Expr::IntToNumber(te) => Expr::IntToNumber(Box::new(transform_typed_expr(te, transform, parser_context))),
        Expr::ModifyAssignment(ao, te1, tle, te2) =>
            Expr::ModifyAssignment(*ao, 
                Box::new(transform_typed_expr(te1, transform, parser_context)), 
                transform_typed_lvalue_expr(tle, transform, parser_context),
                Box::new(transform_typed_expr(te2, transform, parser_context)),
            ),
        Expr::NamedMember(te, s) => Expr::NamedMember(Box::new(transform_typed_expr(te, transform, parser_context)), s.clone()),
        Expr::ObjectLiteral(v) => {
            let mut oles = vec![];
            for ole in v {
                oles.push(ObjectLiteralElem{name: ole.name.clone(), value: transform_typed_expr(&ole.value, transform, parser_context)})
            }
            Expr::ObjectLiteral(oles)
        },
        Expr::Parens(te) => Expr::Parens(Box::new(transform_typed_expr(te, transform, parser_context))),
        Expr::Return(o_te) => Expr::Return(Box::new(o_te.as_ref().as_ref().map(|te| transform_typed_expr(&te, transform, parser_context)))),
        Expr::SizeOf(t) => Expr::SizeOf(t.clone()),
        Expr::StaticFuncCall(s, v) => Expr::StaticFuncCall(s.clone(), transform_typed_exprs(v, transform, parser_context)),
        Expr::TypeLiteral(t) => Expr::TypeLiteral(t.clone()),
        Expr::UnaryOperator{expr: te, op} => Expr::UnaryOperator{expr: Box::new(transform_typed_expr(te, transform, parser_context)), op: *op},
        Expr::VariableDecl(vd) => {
            Expr::VariableDecl(Box::new(VariableDecl{
                internal_name: vd.internal_name.clone(),
                orig_name: vd.orig_name.clone(),
                r#type: vd.r#type.clone(),
                constant: vd.constant,
                init: vd.init.as_ref().map(|te| transform_typed_expr(&te, transform, parser_context)),
                closure_source: vd.closure_source,
                arg: vd.arg
            }))
        },
        Expr::While(te1, te2) => Expr::While(
            Box::new(transform_typed_expr(te1, transform, parser_context)), 
            Box::new(transform_typed_expr(te2, transform, parser_context)), 
        ),
    }
}

pub fn transform_typed_expr(
    typed_expr: &TypedExpr,
    transform: &mut dyn Transform,
    parser_context: &mut dyn ErrRecorder,
) -> TypedExpr {
    let first_pass = transform.transform_typed_expr(typed_expr, parser_context);
    match first_pass{
        Some(out) => return out,
        None => {}
    }
    TypedExpr{
        expr: transform_expr(&typed_expr.expr, transform, &typed_expr.loc, parser_context), 
        is_const: typed_expr.is_const, loc: typed_expr.loc, r#type: typed_expr.r#type.clone()
    }
}