use std::{collections::HashMap, iter};

use crate::{FunctionSignature, PrimitiveType, Type};

pub trait WithBuiltins {
    fn with_builtins() -> Self;
}

impl WithBuiltins for HashMap<String, Type> {
    fn with_builtins() -> Self {
        HashMap::new()
    }
}

impl WithBuiltins for HashMap<String, FunctionSignature> {
    fn with_builtins() -> HashMap<String, FunctionSignature> {
        use PrimitiveType as Ty;
        let num_types = [
            Ty::S8,
            Ty::S16,
            Ty::S32,
            Ty::S64,
            Ty::U8,
            Ty::U16,
            Ty::U32,
            Ty::U64,
            Ty::F32,
            Ty::F64,
        ];
        let float_types = [Ty::F32, Ty::F64];
        let arithmetic = ["add", "sub", "mul", "div", "min", "max"];
        let compare = [
            "eq",
            "greater_eq",
            "greater_than",
            "less_eq",
            "less_than",
            "ne",
        ];
        let float_instructions = ["ceil", "floor", "trunc", "sqrt"];
        let float_operations = float_types.into_iter().flat_map(|ty| {
            float_instructions
                .iter()
                .map(|op| un_op(op, ty, ty))
                .collect::<Vec<_>>()
        });

        let conversions = [
            conv(Ty::S64, Ty::F32),
            conv(Ty::S64, Ty::F64),
            conv(Ty::S32, Ty::F32),
            conv(Ty::S32, Ty::F64),
            conv(Ty::S64, Ty::U32),
            conv(Ty::S64, Ty::S32),
        ];

        num_types
            .into_iter()
            .flat_map(|ty| {
                arithmetic
                    .iter()
                    .map(|op| bin_op(op, ty, ty))
                    .chain(compare.iter().map(|op| bin_op(op, ty, Ty::Bool)))
                    .collect::<Vec<_>>()
            })
            .chain(float_operations)
            .chain(conversions)
            .chain(iter::once(fun(
                "println".to_owned(),
                &[Type::Builtin(Ty::String)],
                Type::None,
            )))
            .chain(collection_operations())
            .collect()
    }
}

fn collection_operations() -> Vec<(String, FunctionSignature)> {
    let collection_types = [
        (Type::List(Box::new(Type::Blank)), "list"),
        (Type::Builtin(PrimitiveType::String), "string"),
    ];

    use PrimitiveType as Ty;
    let primitive_types = [
        Ty::S8,
        Ty::S16,
        Ty::S32,
        Ty::S64,
        Ty::U8,
        Ty::U16,
        Ty::U32,
        Ty::U64,
        Ty::F32,
        Ty::F64,
        Ty::Bool,
        Ty::Char,
        Ty::String,
    ];

    let collection_operations = [("len", Type::Builtin(PrimitiveType::U32))];

    collection_types
        .into_iter()
        .flat_map(|(ty, s)| {
            collection_operations
                .iter()
                .flat_map(|(op, ret)| {
                    primitive_types
                        .iter()
                        .map(|elem| fun(format!("{op}__{s}___{elem}"), &[ty.clone()], ret.clone()))
                        .collect::<Vec<_>>()
                })
                .collect::<Vec<_>>()
        })
        .collect()
}

fn conv(target: PrimitiveType, arg: PrimitiveType) -> (String, FunctionSignature) {
    fun(
        format!("{target}__{arg}"),
        &[Type::Builtin(arg)],
        Type::Builtin(target),
    )
}

fn un_op(op: &str, arg: PrimitiveType, ret: PrimitiveType) -> (String, FunctionSignature) {
    fun(
        format!("{op}__{arg}"),
        &[Type::Builtin(arg), Type::Builtin(arg)],
        Type::Builtin(ret),
    )
}

fn bin_op(op: &str, arg: PrimitiveType, ret: PrimitiveType) -> (String, FunctionSignature) {
    fun(
        format!("{op}__{arg}_{arg}"),
        &[Type::Builtin(arg), Type::Builtin(arg)],
        Type::Builtin(ret),
    )
}

fn fun(name: impl Into<String>, params: &[Type], return_type: Type) -> (String, FunctionSignature) {
    (
        name.into(),
        FunctionSignature {
            param_types: params.into(),
            return_type,
            is_export: false,
        },
    )
}
