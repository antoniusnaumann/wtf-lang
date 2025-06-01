use std::{collections::HashMap, fmt::Display};

use crate::FunctionSignature;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    Never, // will never be constructed, used as the type of return/break/...
    Void,
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
    Record(HashMap<String, Type>),
    Resource {
        methods: HashMap<String, FunctionSignature>,
    },
    Enum {
        cases: Vec<String>,
    },
    Variant {
        cases: HashMap<String, HashMap<String, Type>>,
    },
    Tuple(Vec<Type>),
    Name(String),
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Never => write!(f, "never")?,
            Type::Void => write!(f, "void")?,
            Type::Bool => write!(f, "bool")?,
            Type::Int { signed, bits } => write!(f, "{}{}", if *signed { "s" } else { "u" }, bits)?,
            Type::Float { bits } => write!(f, "f{bits}")?,
            Type::String => write!(f, "string")?,
            Type::Blank => write!(f, "blank")?,
            Type::List(items) => write!(f, "[{}]", items)?,
            Type::Option(payload) => write!(f, "({payload})?")?,
            Type::Result { ok, err } => write!(f, "({ok})!({err})")?,
            Type::Record(hash_map) => {
                write!(f, "{{")?;
                let mut first = true;
                for (key, value) in hash_map {
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
                    for (name, ty) in payloads {
                        write!(f, "{name}: {ty}")?;
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
            Type::Name(name) => write!(f, "{name}")?,
        };

        Ok(())
    }
}

pub fn unify(a: &Type, b: &Type) -> Type {
    match (a, b) {
        (Type::Blank, a) | (a, Type::Blank) => a.clone(),
        (Type::Never, a) | (a, Type::Never) => a.clone(),
        (Type::Void, Type::Void) => Type::Void,
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
        (Type::Void, Type::Option(a)) | (Type::Option(a), Type::Void) => Type::Option(a.clone()),
        (Type::Void, a) | (a, Type::Void) => Type::Option(a.clone().into()),
        (Type::String, Type::String) => Type::String,
        (Type::List(item_a), Type::List(item_b)) => Type::List(Box::new(unify(item_a, item_b))),
        (Type::Record(fields_a), Type::Record(fields_b)) => {
            todo!("compare records")
        }
        (Type::Variant { cases: cases_a }, Type::Variant { cases: cases_b }) => {
            todo!("compare cases_a and cases_b")
        }
        (Type::Name(name_a), Type::Name(name_b)) => {
            if name_a == name_b {
                a.clone()
            } else {
                todo!("resolve names and compare types")
            }
        }
        (a, b) => panic!("incompatible types {a} and {b}"),
    }
}
