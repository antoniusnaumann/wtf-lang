use std::{collections::HashMap, fmt::Display};

use crate::FunctionSignature;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    Never, // will never be constructed, used as the type of return/break/...
    None,
    Bool,
    Char,
    Int {
        signed: bool,
        bits: usize,
    },
    Float {
        bits: usize,
    },
    String,
    Blank, // for builtin generic types where the type does not matter, e.g. list.len()
    List(Box<Type>),
    Option(Box<Type>),
    Result {
        ok: Box<Type>,
        err: Box<Type>,
    },
    Record(Vec<(String, Type)>),
    Resource {
        methods: HashMap<String, FunctionSignature>,
    },
    Enum {
        cases: Vec<String>,
    },
    Variant {
        cases: HashMap<String, Vec<(String, Type)>>,
    },
    Tuple(Vec<Type>),
    Meta(Box<Type>),
}

impl Type {
    pub const fn u32() -> Type {
        Type::Int {
            signed: false,
            bits: 32,
        }
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Never => write!(f, "never")?,
            Type::None => write!(f, "void")?,
            Type::Bool => write!(f, "bool")?,
            Type::Int { signed, bits } => write!(f, "{}{}", if *signed { "s" } else { "u" }, bits)?,
            Type::Float { bits } => write!(f, "f{bits}")?,
            Type::String => write!(f, "string")?,
            Type::Blank => write!(f, "blank")?,
            Type::List(items) => write!(f, "[{}]", items)?,
            Type::Option(payload) => write!(f, "({payload})?")?,
            Type::Result { ok, err } => write!(f, "({ok})!({err})")?,
            Type::Record(fields) => {
                write!(f, "{{")?;
                let mut first = true;
                for (key, value) in fields {
                    if first {
                        first = false;
                    } else {
                        write!(f, ", ")?;
                    }
                    write!(f, "{key}: {value}")?;
                }
                write!(f, "}}")?
            }
            Type::Resource { methods } => write!(f, "...")?,
            Type::Enum { cases } => {
                let mut first = true;
                for case in cases {
                    if first {
                        first = false;
                    } else {
                        write!(f, " | ")?;
                    }
                    write!(f, "{case}")?;
                }
            }
            Type::Variant { cases } => {
                let mut first = true;
                for (name, payloads) in cases {
                    if first {
                        first = false;
                    } else {
                        write!(f, " | ")?;
                    }
                    write!(f, "{name}(")?;
                    let mut payload_first = true;
                    for (field_name, ty) in payloads {
                        if payload_first {
                            payload_first = false;
                        } else {
                            write!(f, ", ")?;
                        }
                        write!(f, "{field_name}: {ty}")?;
                    }
                    write!(f, ")")?;
                }
            }
            Type::Tuple(payloads) => {
                write!(f, "(")?;
                let mut first = true;
                for payload in payloads {
                    if first {
                        first = false;
                    } else {
                        write!(f, ", ")?;
                    }
                    write!(f, "{payload}")?;
                }
                write!(f, ")")?
            }
            Type::Char => write!(f, "char")?,
            Type::Meta(ty) => write!(f, "meta({ty})")?,
        };

        Ok(())
    }
}

pub fn unify(a: &Type, b: &Type) -> Type {
    match (a, b) {
        (Type::Blank, a) | (a, Type::Blank) => a.clone(),
        (Type::Never, a) | (a, Type::Never) => a.clone(),
        (Type::None, Type::None) => Type::None,
        (
            Type::Int {
                signed: signed_a,
                bits: bits_a,
            },
            Type::Int {
                signed: signed_b,
                bits: bits_b,
            },
        ) => {
            if signed_a == signed_b && bits_a == bits_b {
                a.clone()
            } else {
                panic!("incompatible int types")
            }
        }
        (Type::None, Type::Option(a)) | (Type::Option(a), Type::None) => Type::Option(a.clone()),
        (Type::None, a) | (a, Type::None) => Type::Option(a.clone().into()),
        (Type::String, Type::String) => Type::String,
        (Type::List(item_a), Type::List(item_b)) => Type::List(Box::new(unify(item_a, item_b))),
        (Type::Record(fields_a), Type::Record(fields_b)) => {
            // For now, check if they have the same fields in the same order
            // This could be improved to be more flexible with field ordering
            if fields_a.len() != fields_b.len() {
                panic!("incompatible record types: different number of fields")
            }
            let mut unified_fields = Vec::new();
            for ((name_a, type_a), (name_b, type_b)) in fields_a.iter().zip(fields_b.iter()) {
                if name_a != name_b {
                    panic!("incompatible record types: different field names")
                }
                unified_fields.push((name_a.clone(), unify(type_a, type_b)));
            }
            Type::Record(unified_fields)
        }
        (Type::Variant { cases: cases_a }, Type::Variant { cases: cases_b }) => {
            todo!("compare cases_a and cases_b")
        }
        (a, b) => panic!("incompatible types {a} and {b}"),
    }
}
