use wa1_ast::prelude::*;
pub use wa1_errs::Error;
pub use wa1_errs::prelude::*;

pub trait Transform{
    fn transform_typed_expr(&mut self, typed_expr: &TypedExpr, err_recorder: &mut dyn ErrRecorder) -> Option<TypedExpr>;
    fn transform_expr(&mut self, expr: &Expr, loc: &SourceLocation, err_recorder: &mut dyn ErrRecorder) -> Option<Expr>;
    fn transform_typed_lvalue_expr(&mut self, typed_lvalue_expr: &TypedLValueExpr, err_recorder: &mut dyn ErrRecorder) -> Option<TypedLValueExpr>;
    fn transform_lvalue_expr(&mut self, lvalue_expr: &LValueExpr, loc: &SourceLocation, err_recorder: &mut dyn ErrRecorder) -> Option<LValueExpr>;
    fn transform_func_decl(&mut self, lvalue_expr: &FuncDecl, loc: &SourceLocation, err_recorder: &mut dyn ErrRecorder) -> Option<FuncDecl>;
}

pub fn transform_func_decl(
    fund_decl: &FuncDecl,
    transform: &mut dyn Transform,
    loc: &SourceLocation, 
    parser_context: &mut dyn ErrRecorder,
) -> FuncDecl {
    let first_pass = transform.transform_func_decl(fund_decl, loc, parser_context);
    match first_pass{
        Some(out) => return out,
        None => {}
    }

    FuncDecl{name: fund_decl.name.clone(), return_type: fund_decl.return_type.clone(), args: fund_decl.args.clone(), export: fund_decl.export, 
        generic_impl: fund_decl.generic_impl, type_guard: fund_decl.type_guard.clone(), member_func: fund_decl.member_func}
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
        LValueExpr::StaticNamedMemberAssign(te, t, s) => LValueExpr::StaticNamedMemberAssign(
            Box::new(transform_typed_expr(te, transform, parser_context)),
            t.clone(),
            s.clone(),
        ),
        LValueExpr::DynamicMemberAssign(te1, t, te2) => LValueExpr::DynamicMemberAssign(
            Box::new(transform_typed_expr(te1, transform, parser_context)),
            t.clone(),
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
        Expr::BoolLiteral(_) | Expr::Break | Expr::ClassDecl(_) | Expr::ClosureVariableUse(_) 
            | Expr::Continue | Expr::FloatLiteral(_) | Expr::GlobalVariableUse(_) | Expr::IntLiteral(_) 
            | Expr::LocalVariableUse(_) | Expr::NoOp | Expr::Null | Expr::StringLiteral(_)
            | Expr::UnsafeNull | Expr::Void
            => expr.clone(),

        Expr::SimpleAssignment{l_value, r_value} => {
            Expr::SimpleAssignment{
                l_value: transform_typed_lvalue_expr(l_value, transform, parser_context),
                r_value: Box::new(transform_typed_expr(r_value, transform, parser_context)),
            }
        },
        Expr::UnresolvedAssignment{l_value, r_value} => {
            Expr::UnresolvedAssignment{
                l_value: transform_typed_lvalue_expr(l_value, transform, parser_context),
                r_value: Box::new(transform_typed_expr(r_value, transform, parser_context)),
            }
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
        Expr::FreeUserTypeWrap(te) => Expr::FreeUserTypeWrap(Box::new(transform_typed_expr(te, transform, parser_context))),
        Expr::FreeUserTypeUnwrap(te) => Expr::FreeUserTypeUnwrap(Box::new(transform_typed_expr(te, transform, parser_context))),
        Expr::FreeDowncast(te) => Expr::FreeDowncast(Box::new(transform_typed_expr(te, transform, parser_context))),
        Expr::FreeGenericCast(te) => Expr::FreeGenericCast(Box::new(transform_typed_expr(te, transform, parser_context))),
        Expr::FreeUpcast(te) => Expr::FreeUpcast(Box::new(transform_typed_expr(te, transform, parser_context))),
        Expr::FuncDecl(foc) => Expr::FuncDecl(foc.clone()),
        Expr::SimpleGlobalVariableDecl(gvd) => 
            Expr::SimpleGlobalVariableDecl(Box::new(GlobalVariableDecl{name: gvd.name.clone(), r#type: gvd.r#type.clone(), export: gvd.export, 
                init: gvd.init.as_ref().map(|e| transform_typed_expr(&e, transform, parser_context)), mutability: gvd.mutability
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
                Intrinsic::MemoryGrow(te) => Expr::Intrinsic(Intrinsic::MemoryGrow(Box::new(transform_typed_expr(te, transform, parser_context)))),
                Intrinsic::MemorySize => Expr::Intrinsic(Intrinsic::MemorySize),
                Intrinsic::Trap => Expr::Intrinsic(Intrinsic::Trap),
            }
        },
        Expr::IntWiden(te) => Expr::IntWiden(Box::new(transform_typed_expr(te, transform, parser_context))),
        Expr::IntToNumber(te) => Expr::IntToNumber(Box::new(transform_typed_expr(te, transform, parser_context))),
        Expr::MemberFuncCall(te, s, vs) => Expr::MemberFuncCall(Box::new(transform_typed_expr(te, transform, parser_context)), s.clone(), transform_typed_exprs(vs, transform, parser_context)),
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
        Expr::StaticFuncCall(s, fd, v) => Expr::StaticFuncCall(s.clone(), transform_func_decl(fd, transform, loc, parser_context),
            transform_typed_exprs(v, transform, parser_context)),
        Expr::TemporaryCreation(te, n) => Expr::TemporaryCreation(Box::new(transform_typed_expr(te, transform, parser_context)), n.clone()),
        Expr::TupleLiteral(v) => Expr::TupleLiteral(transform_typed_exprs(v, transform, parser_context)),
        Expr::TraitMemberFuncCall{trait_member_func, this_expr, args} => Expr::TraitMemberFuncCall{
            this_expr: Box::new(transform_typed_expr(this_expr, transform, parser_context)),
            args: transform_typed_exprs(args, transform, parser_context),
            trait_member_func: trait_member_func.clone()
        },
        Expr::TypeLiteral(t) => Expr::TypeLiteral(t.clone()),
        Expr::UnaryOperator{expr: te, op} => Expr::UnaryOperator{expr: Box::new(transform_typed_expr(te, transform, parser_context)), op: *op},
        Expr::UnsafeSome(te) => Expr::UnsafeSome(Box::new(transform_typed_expr(te, transform, parser_context))),
        Expr::UnresolvedGenericFuncCall{name, unresolved_func_decl, args, unresolved_types} => 
            Expr::UnresolvedGenericFuncCall{name: name.clone(), unresolved_func_decl: transform_func_decl(unresolved_func_decl, transform, loc, parser_context),
                args: transform_typed_exprs(args, transform, parser_context), unresolved_types: unresolved_types.clone()
            },
        Expr::SimpleVariableInit{internal_name, init} => {
            Expr::SimpleVariableInit{
                internal_name: internal_name.clone(),
                init: Box::new(transform_typed_expr(&init, transform, parser_context)),
            }
        },
        Expr::UnresolvedVariableInit{internal_name, var_type, init} => {
            Expr::UnresolvedVariableInit{
                internal_name: internal_name.clone(),
                var_type: var_type.clone(),
                init: Box::new(transform_typed_expr(&init, transform, parser_context)),
            }
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
        loc: typed_expr.loc, r#type: typed_expr.r#type.clone(), return_expr: ReturnExpr::None
    }
}