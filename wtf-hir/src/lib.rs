mod compiler;
mod visible;

use std::{collections::{HashMap, HashSet}, fmt::Display};
pub use compiler::compile;

#[derive(Debug, Clone, PartialEq)]
pub struct Module {
    pub types: HashMap<String, Type>,
    pub functions: HashMap<String, Function>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    Never, // will never be constructed, used as the type of return/break/...
    None,
    S8,
    S16,
    S32,
    S64,
    U8,
    U16,
    U32,
    U64,
    F32,
    F64,
    // bool is lowered to an enum with true and false
    Char,
    String,
    List(Box<Type>),
    Option(Box<Type>),
    Result {
        ok: Box<Type>,
        err: Box<Type>,
    },
    Record(HashMap<String, Type>),
    Resource(ResourceType),
    Enum(HashSet<String>),
    Variant(HashMap<String, HashMap<String, Type>>),
    Tuple(Vec<Type>),
}
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ResourceType {
    pub methods: HashMap<String, FunctionSignature>,
}
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FunctionSignature {
    param_types: Vec<Type>,
    return_type: Type,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Function {
    pub parameters: Vec<(String, Type)>,
    pub return_type: Type,
    pub expressions: Vec<Expression>,
    pub body: Id,
}

#[derive(Debug, Clone, PartialEq, Copy)]
pub struct Id(usize);

#[derive(Debug, Clone, PartialEq)]
pub struct Expression {
    type_: Type,
    kind: ExpressionKind,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ExpressionKind {
    Param,
    Block {
        children: Vec<Id>,
        result: Id,
    },
    Int(i64),
    Float(f64),
    String(String),
    None,
    Enum {
        variant: String,
        payload: Id,
    },
    Record {
        fields: HashMap<String, Id>,
    },
    ListLiteral(Vec<Id>),
    FunctionCall {
        function: String,
        arguments: Vec<Id>,
    },
    FieldAccess {
        receiver: Id,
        field: String,
    },
    IndexAccess {
        collection: Id,
        index: Id,
    },
    Assignment {
        target: Id,
        value: Id,
    },
    Return(Id),
    Break(Id),
    Continue,
    Throw(Id),
    If {
        condition: Id,
        then: Id,
        else_: Id,
    },
    Match {
        condition: Id,
        arms: HashMap<String, Id>,
    },
    Loop(Id),
}

impl Expression {
    fn param(type_: Type) -> Self {
        Self {
            type_, kind: ExpressionKind::Param
        }
    }
    fn block(children: Vec<Id>, result: Id, result_type: Type) -> Self {
        Self {
            type_: result_type,
            kind: ExpressionKind::Block { children, result }
        }
    }
    fn int(int: i64) -> Self {
        Self {
            type_: Type::S64,
            kind: ExpressionKind::Int(int)
        }
    }
    fn float(float: f64) -> Self {
        Self {
            type_: Type::F64,
            kind: ExpressionKind::Float(float)
        }
    }
    fn string(string: String) -> Self {
        Self {
            type_: Type::String,
            kind: ExpressionKind::String(string)
        }
    }
    fn bool(b: bool, none: Id) -> Self {
        Self {
            type_: Type::Enum(HashSet::from_iter(["True".to_string(), "String".to_string()].into_iter())),
            kind: ExpressionKind::Enum { variant: if b { "true".to_string() } else { "false".to_string() }, payload: none }
        }
    }
    fn none() -> Self {
        Self {
            type_: Type::None,
            kind: ExpressionKind::None
        }
    }
    fn return_(what: Id) -> Self {
        Self {
            type_: Type::Never,
            kind: ExpressionKind::Return(what)
        }
    }
    fn break_(id: Id) -> Self {
        Self {
            type_: Type::Never,
            kind: ExpressionKind::Break(id)
        }
    }
    fn continue_() -> Self {
        Self {
            type_: Type::Never,
            kind: ExpressionKind::Continue
        }
    }
    fn if_(condition: Id, then: Id, else_: Id) -> Self {
        // TODO: Join types.
        Self {
            type_: Type::None,
            kind: ExpressionKind::If { condition, then, else_ }
        }
    }
    fn loop_(body: Id) -> Self {
        // TODO: join types of breaks
        Self {
            type_: Type::Never,
            kind: ExpressionKind::Loop(body),
        }
    }
    fn call(function: String, arguments: Vec<Id>) -> Self {
        Self {
            type_: Type::Never,
            kind: ExpressionKind::FunctionCall { function, arguments }
        }
    }
}

pub fn get(exprs: &Vec<Expression>, id: Id) -> Expression {
    exprs[id.0].clone()
}

impl Display for Module {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "Type:")?;
        for (name, type_) in &self.types {
            writeln!(f, "{name} = {type_}")?;
        }
        writeln!(f, "Functions:")?;
        for (name, function) in &self.functions {
            write!(f, "fun {}", name)?;
            for (name, type_) in &function.parameters {
                write!(f, " {}: ({})", name, type_)?;
            }
            write!(f, " -> {} ", function.return_type)?;
            get(&function.expressions, function.body).fmt(f, 0, &function.expressions)?;
        }
        Ok(())
    }
}
impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Never => write!(f, "Never")?,
            Type::None => write!(f, "None")?,
            Type::S8 => write!(f, "S8")?,
            Type::S16 => write!(f, "S16")?,
            Type::S32 => write!(f, "S32")?,
            Type::S64 => write!(f, "S64")?,
            Type::U8 => write!(f, "U8")?,
            Type::U16 => write!(f, "U16")?,
            Type::U32 => write!(f, "U32")?,
            Type::U64 => write!(f, "U64")?,
            Type::F32 => write!(f, "F32")?,
            Type::F64 => write!(f, "F64")?,
            Type::Char => write!(f, "Char")?,
            Type::String => write!(f, "String")?,
            Type::List(items) => write!(f, "[{}]", items)?,
            Type::Option(payload) => write!(f, "?({payload})")?,
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
            },
            Type::Resource(_) => write!(f, "...")?,
            Type::Enum(variants) => {
                let mut first = true;
                for variant in variants {
                    if first {
                        first = false;
                    } else {
                        write!(f, " | ")?;
                    }
                    write!(f, "{variant}")?;
                }
            },
            Type::Variant(variants) => {
                let mut first = true;
                for (name, payloads) in variants {
                    if first {
                        first = false;
                    } else {
                        write!(f, " | ")?;
                    }
                    write!(f, "{name}(")?;
                    for (name, type_) in payloads {
                        write!(f, "{name}: {type_}")?;
                    }
                    write!(f, ")")?;
                }
            },
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
            },
        };
        Ok(())
    }
}
impl Display for Id {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "@{}", self.0)
    }
}
impl Id {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>, indentation: usize, exprs: &Vec<Expression>) -> std::fmt::Result {
        for _ in 0..indentation {
            write!(f, "  ")?;
        }
        let expr = get(exprs, *self);
        write!(f, "{self}: {} = ", expr.type_)?;
        expr.fmt(f, indentation, exprs)?;
        writeln!(f, "")?;
        Ok(())
    }
}
impl Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>, indentation: usize, exprs: &Vec<Expression>) -> std::fmt::Result {
        match &self.kind {
            ExpressionKind::Param => write!(f, "param")?,
            ExpressionKind::Block { children, result } => {
                writeln!(f, "{{")?;
                for child in children {
                    child.fmt(f, indentation + 1, exprs)?;
                }
                for _ in 0..=indentation {
                    write!(f, "  ")?;
                }
                writeln!(f, "{result}")?;
                for _ in 0..indentation {
                    write!(f, "  ")?;
                }
                write!(f, "}}")?;
                return Ok(())
            },
            ExpressionKind::Int(int) => write!(f, "{}", int)?,
            ExpressionKind::Float(float) => write!(f, "{}", float)?,
            ExpressionKind::String(string) => write!(f, "{:?}", string)?,
            ExpressionKind::None => write!(f, "none")?,
            ExpressionKind::Enum { variant, payload } => write!(f, "{}({})", variant, payload)?,
            ExpressionKind::Record { fields } => {
                write!(f, "{{")?;
                let mut first = true;
                for (name, value) in fields {
                    if first {
                        first = false;
                    } else {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}: {}", name, value)?;
                }
                write!(f, "}}")?;
            },
            ExpressionKind::ListLiteral(items) => {
                write!(f, "[")?;
                let mut first = true;
                for item in items {
                    if first {
                        first = false;
                    } else {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", item)?;
                }
                write!(f, "]")?;
            },
            ExpressionKind::FunctionCall { function, arguments } => {
                write!(f, "{function}")?;
                for arg in arguments {
                    write!(f, " {}", arg)?;
                }
            },
            ExpressionKind::FieldAccess { receiver, field } => write!(f, "{receiver}.{field}")?,
            ExpressionKind::IndexAccess { collection, index } => write!(f, "{collection}[{index}]")?,
            ExpressionKind::Assignment { target, value } => write!(f, "{target} = {value}")?,
            ExpressionKind::Return(id) => write!(f, "return {id}")?,
            ExpressionKind::Break(id) => write!(f, "break {id}")?,
            ExpressionKind::Continue => write!(f, "continue")?,
            ExpressionKind::Throw(id) => write!(f, "throw {id}")?,
            ExpressionKind::If { condition, then, else_ } => {
                write!(f, "if {condition} ")?;
                get(exprs, *then).fmt(f, indentation, exprs)?;
                write!(f, " else ")?;
                get(exprs, *else_).fmt(f, indentation, exprs)?;
            },
            ExpressionKind::Match { condition, arms } => {
                write!(f, "match {condition} {{")?;
                for (variant, arm) in arms {
                    write!(f, "  {variant} => {arm}")?;
                }
                write!(f, "}}")?;
            },
            ExpressionKind::Loop(body) => write!(f, "loop {body}")?,
        }
        Ok(())
    }
}
