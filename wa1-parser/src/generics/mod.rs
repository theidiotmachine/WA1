use std::collections::{HashMap};

use wa1_types::prelude::*;
pub use wa1_errs::Error;
pub use wa1_errs::prelude::*;

fn transform_full_types(ts: &Vec<FullType>,
    type_map: &HashMap<String, Type>,
    loc: &SourceLocation,
    parser_context: &mut dyn ErrRecorder,
) -> Vec<FullType> {
    let mut out = vec![];
    for t in ts {
        out.push(FullType::new(&transform_type(&t.r#type, type_map, loc, parser_context), t.mutability));
    }
    out
}

pub fn transform_func_type(func_type: &FuncType,
    type_map: &HashMap<String, Type>,
    loc: &SourceLocation,
    parser_context: &mut dyn ErrRecorder,
) -> FuncType {
    FuncType{
        out_type: FullType::new(&transform_type(&func_type.out_type.r#type, type_map, loc, parser_context), func_type.out_type.mutability),
        in_types: transform_full_types(&func_type.in_types, type_map, loc, parser_context)
    }
}

fn transform_types(ts: &Vec<Type>,
    type_map: &HashMap<String, Type>,
    loc: &SourceLocation,
    parser_context: &mut dyn ErrRecorder,
) -> Vec<Type> {
    let mut out = vec![];
    for t in ts {
        out.push(transform_type(t, type_map, loc, parser_context));
    }
    out
}

/// Substitute all type variables for their concrete values
pub fn transform_type(t: &Type,
    type_map: &HashMap<String, Type>,
    loc: &SourceLocation,
    parser_context: &mut dyn ErrRecorder,
) -> Type {
    match t {
        Type::Any | Type::Bool | Type::FakeVoid | Type::FloatLiteral(_) 
        | Type::Int(_, _) | Type::ModuleLiteral(_) | Type::Never | Type::Number | Type::RealVoid | Type::String 
        | Type::StringLiteral(_) | Type::Undeclared | Type::Unknown | Type::UnsafePtr | Type::UnsafeNull
        | Type::UnsafeStruct{name: _}
            => t.clone(),
        Type::Array(t) => Type::Array(Box::new(transform_type(t, type_map, loc, parser_context))),
        Type::Func{func_type} => {
            Type::Func{func_type: Box::new(transform_func_type(func_type, type_map, loc, parser_context))}
        },
        Type::ObjectLiteral(oles) => {
            Type::ObjectLiteral(oles.iter().map(|(k, v)| (k.clone(), transform_type(&v, type_map, loc, parser_context))).collect())
        },
        Type::Option(t) => Type::Option(Box::new(transform_type(&t, type_map, loc, parser_context))),
        Type::Some(t) => Type::Some(Box::new(transform_type(&t, type_map, loc, parser_context))),
        Type::Tuple(ts) => Type::Tuple(transform_types(&ts, type_map, loc, parser_context)),
        Type::TypeLiteral(t) => Type::TypeLiteral(Box::new(FullType::new(&transform_type(&t.r#type, type_map, loc, parser_context), t.mutability))),
        Type::UnsafeArray(t) => Type::UnsafeArray(Box::new(transform_type(t, type_map, loc, parser_context))),
        Type::UnsafeOption(t) => Type::UnsafeOption(Box::new(transform_type(&t, type_map, loc, parser_context))),
        Type::UnsafeSome(t) => Type::UnsafeSome(Box::new(transform_type(&t, type_map, loc, parser_context))),
        Type::UserClass{name, type_args} => 
            Type::UserClass{name: name.clone(), type_args: transform_types(&type_args, type_map, loc, parser_context)},
        Type::UserType{name, type_args, inner} => 
            Type::UserType{name: name.clone(), type_args: transform_types(&type_args, type_map, loc, parser_context), inner: Box::new(transform_type(&inner, type_map, loc, parser_context))},
        Type::VariableUsage{name, constraint: _} => {
            let o_n_t = type_map.get(name);
            match o_n_t {
                Some(n_t) => {
                    (*n_t).clone()
                },
                None => {
                    parser_context.push_err(Error::UnresolvedTypeArg(loc.clone(), name.clone()));
                    t.clone()
                }
            }
        },
    }
}


/// Helper to create a type map, which is a map from type arg name to type. Starts off all undeclared
pub fn new_type_map(type_args: &Vec<TypeArg>) -> HashMap<String, Type> {
    let mut resolved_type_map: HashMap<String, Type> = HashMap::new();
    for type_arg in type_args {
        resolved_type_map.insert(type_arg.name.clone(), Type::Undeclared);
    }
    resolved_type_map
}