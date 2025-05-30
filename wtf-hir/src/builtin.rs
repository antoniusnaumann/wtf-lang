use std::{collections::HashMap, iter};

use crate::{FunctionSignature, Type};

pub trait WithBuiltins {
    fn with_builtins() -> Self;
}

impl WithBuiltins for HashMap<String, Type> {
    fn with_builtins() -> Self {
        HashMap::new()
    }
}

fn s(bits: usize) -> Type {
    Type::Int { signed: true, bits }
}

fn u(bits: usize) -> Type {
    Type::Int {
        signed: false,
        bits,
    }
}

fn f(bits: usize) -> Type {
    Type::Float { bits }
}

impl WithBuiltins for HashMap<String, FunctionSignature> {
    fn with_builtins() -> HashMap<String, FunctionSignature> {
        use Type as Ty;
        let num_types: Vec<_> = [8usize, 16, 32, 64]
            .iter()
            .flat_map(|&bits| {
                [
                    Ty::Int { signed: true, bits },
                    Ty::Int {
                        signed: false,
                        bits,
                    },
                ]
            })
            .chain([Ty::Float { bits: 32 }, Ty::Float { bits: 64 }])
            .collect();
        let float_types = [Ty::Float { bits: 32 }, Ty::Float { bits: 64 }];
        // TODO: make these methods pseudo-generic / generate them on the fly when the types fulfill their requirements
        let arithmetic = ["add", "sub", "mul", "div", "min", "max"];
        let compare = [
            "eq",
            "greater_eq",
            "greater_than",
            "less_eq",
            "less_than",
            "ne",
        ];
        let logical_operations = [
            bin_op("and", Ty::Bool, Ty::Bool),
            bin_op("or", Ty::Bool, Ty::Bool),
        ];
        let float_instructions = ["ceil", "floor", "trunc", "sqrt"];
        let float_operations = float_types.into_iter().flat_map(|ty| {
            float_instructions
                .iter()
                .map(|op| un_op(op, ty.clone(), ty.clone()))
                .collect::<Vec<_>>()
        });

        let conversions = [
            conv(s(64), f(32)),
            conv(s(64), f(64)),
            conv(s(32), f(32)),
            conv(s(32), f(64)),
            conv(s(32), u(32)),
            conv(s(64), u(32)),
            conv(s(64), s(32)),
        ];

        num_types
            .into_iter()
            .flat_map(|ty| {
                arithmetic
                    .iter()
                    .map(|op| bin_op(op, ty.clone(), ty.clone()))
                    .chain(compare.iter().map(|op| bin_op(op, ty.clone(), Ty::Bool)))
                    .collect::<Vec<_>>()
            })
            .chain(float_operations)
            .chain(logical_operations)
            .chain(conversions)
            .chain(iter::once(fun(
                "println".to_owned(),
                &[Ty::String],
                Type::Void,
            )))
            .chain(iter::once(fun(
                "is_some".to_owned(),
                &[Type::Option(Box::new(Type::Blank))],
                Type::Blank,
            )))
            .chain(collection_operations())
            .collect()
    }
}

fn collection_operations() -> Vec<(String, FunctionSignature)> {
    use Type as Ty;
    let primitive_types = [
        s(8),
        s(16),
        s(32),
        s(64),
        u(8),
        u(16),
        u(32),
        u(64),
        f(32),
        f(64),
        Ty::Bool,
        Ty::Char,
        Ty::String,
    ];

    let collection_operations = [("len", u(32))];

    let mut funs = Vec::new();

    for (name, ty) in collection_operations {
        funs.push(fun(format!("{name}__string"), &[Type::String], ty.clone()));

        for elem in &primitive_types {
            funs.push(fun(
                format!("{name}__list___{elem}"),
                &[Type::List(elem.to_owned().into())],
                ty.clone(),
            ));
        }
    }

    funs
}

fn conv(target: Type, arg: Type) -> (String, FunctionSignature) {
    fun(format!("{target}__{arg}"), &[arg], target)
}

fn un_op(op: &str, arg: Type, ret: Type) -> (String, FunctionSignature) {
    fun(format!("{op}__{arg}"), &[arg.clone(), arg], ret)
}

fn bin_op(op: &str, arg: Type, ret: Type) -> (String, FunctionSignature) {
    fun(format!("{op}__{arg}_{arg}"), &[arg.clone(), arg], ret)
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
