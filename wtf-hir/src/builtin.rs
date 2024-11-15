use std::collections::HashMap;

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
        ];
        let arithmetic = ["add", "sub", "mul", "div"];
        let compare = [
            "eq",
            "greater_eq",
            "greater_than",
            "less_eq",
            "less_than",
            "ne",
        ];
        num_types
            .into_iter()
            .flat_map(|ty| {
                arithmetic
                    .iter()
                    .map(|op| {
                        fun(
                            format!("{op}__{ty}_{ty}"),
                            &[Type::Builtin(ty), Type::Builtin(ty)],
                            Type::Builtin(ty),
                        )
                    })
                    .chain(compare.iter().map(|op| {
                        fun(
                            format!("{op}__{ty}_{ty}"),
                            &[Type::Builtin(ty), Type::Builtin(ty)],
                            Type::Builtin(Ty::Bool),
                        )
                    }))
                    .collect::<Vec<_>>()
            })
            .collect()
    }
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
