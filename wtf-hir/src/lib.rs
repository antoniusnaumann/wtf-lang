// mod compiler;

// pub use compiler::compile;

use std::collections::{HashMap, HashSet};

#[derive(Debug, Clone, PartialEq)]
pub struct Module {
    // TODO: How should we mark exported types and functions here?
    pub types: HashMap<String, Type>,
    pub functions: HashMap<String, Function>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    List(Box<Type>),
    Option(Box<Type>),
    Result { ok: Box<Type>, err: Box<Type> },
    Record(HashMap<String, Type>),
    Resource(ResourceType),
    Enum(HashSet<String>),
    Variant(HashMap<String, Type>),
    Tuple(Vec<Type>),
}
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ResourceType {
    pub methods: HashMap<String, FunctionSignature>,
}
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FunctionSignature {
    param_types: Vec<Type>,
    return_type: Option<Type>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Function {
    pub parameters: Vec<(String, Type)>,
    pub return_type: Option<Type>,
    pub expressions: Vec<Expression>,
    pub body: Id,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Id(usize);

#[derive(Debug, Clone, PartialEq)]
pub struct Expression {
    type_: Type,
    value: ExpressionKind,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ExpressionKind {
    Block(Vec<Id>),
    Integer(i64),
    Float(f64),
    String(String),
    Boolean(bool),
    None,
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
