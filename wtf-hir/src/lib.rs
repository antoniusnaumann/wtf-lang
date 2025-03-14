mod builtin;
mod compiler;
mod visible;

pub use compiler::compile;
use std::{
    collections::{HashMap, HashSet},
    fmt::Display,
};

#[derive(Debug, Clone, PartialEq)]
pub struct Module {
    pub types: HashMap<String, Type>,
    pub functions: HashMap<String, Function>,
    pub tests: Vec<Test>,
    pub constants: HashSet<Vec<u8>>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    Never, // will never be constructed, used as the type of return/break/...
    Void,
    Blank, // for builtin generic types where the type does not matter, e.g. list.len()
    List(Box<Type>),
    Option(Box<Type>),
    Result { ok: Box<Type>, err: Box<Type> },
    Record(HashMap<String, Type>),
    Resource(ResourceType),
    Enum(EnumType),
    Variant(HashMap<String, HashMap<String, Type>>),
    Tuple(Vec<Type>),
    Builtin(PrimitiveType),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EnumType {
    pub cases: Vec<String>,
    pub name: String,
}

impl From<EnumType> for Type {
    fn from(value: EnumType) -> Self {
        Type::Enum(value)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum PrimitiveType {
    Bool,
    S8,
    U8,
    S16,
    U16,
    S32,
    U32,
    S64,
    U64,
    F32,
    F64,
    Char,
    String,
}

impl PrimitiveType {
    pub fn name(&self) -> &str {
        match self {
            PrimitiveType::Bool => "bool",
            PrimitiveType::S8 => "s8",
            PrimitiveType::U8 => "u8",
            PrimitiveType::S16 => "s16",
            PrimitiveType::U16 => "u16",
            PrimitiveType::S32 => "s32",
            PrimitiveType::U32 => "u32",
            PrimitiveType::S64 => "s64",
            PrimitiveType::U64 => "u64",
            PrimitiveType::F32 => "f32",
            PrimitiveType::F64 => "f64",
            PrimitiveType::Char => "char",
            PrimitiveType::String => "string",
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ResourceType {
    pub methods: HashMap<String, FunctionSignature>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FunctionSignature {
    param_types: Vec<Type>,
    return_type: Type,
    is_export: bool,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Function {
    pub parameters: Vec<(String, Type)>,
    pub return_type: Type,
    pub locals: Vec<Type>, // include parameters
    pub body: Block,
    pub is_export: bool,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Test {
    pub name: Option<String>,
    pub id: String,
    pub locals: Vec<Type>, // include parameters
    pub body: Block,
}

#[derive(Debug, Clone, PartialEq, Copy)]
pub struct LocalId(pub usize);

#[derive(Debug, Clone, PartialEq, Copy)]
pub struct Id(usize);

#[derive(Debug, Clone, PartialEq)]
pub struct Block {
    pub instructions: Vec<Instruction>,
    pub ty: Type,
}

impl Block {
    fn new() -> Self {
        Self::default()
    }
}

impl Default for Block {
    fn default() -> Self {
        Block {
            instructions: Vec::new(),
            ty: Type::Void,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Instruction {
    Pop,
    Load(LocalId),
    Store(LocalId),
    Int(i64),
    Float(f64),
    String(String),
    Bool(bool),
    Void,
    Enum {
        case: usize,
        ty: EnumType,
    },
    Variant {
        case: String,
        num_payloads: usize,
    },
    // TODO: @marcel: we assume from here on that this is already in declaration field order
    Record(Vec<String>),
    Option {
        is_some: bool,
        ty: Type,
    },
    List {
        len: usize,
        ty: Type,
    },
    Call {
        function: String,
        num_arguments: usize,
    },
    // Access field of current stack value
    FieldAccess(String),
    // Optimized field access for locals
    MemberChain(LocalId, Vec<String>),
    IndexAccess {
        ty: Type,
    },
    Unreachable,
    Return,
    Break,
    Continue,
    Throw,
    If {
        then: Block,
        else_: Block,
    },
    Match {
        arms: HashMap<String, Block>,
    },
    Loop(Block),
}

// impl Expression {
//     fn param(type_: Type) -> Self {
//         Self {
//             type_, kind: Instruction::Param
//         }
//     }
//     fn block(children: Vec<Id>, result: Id, result_type: Type) -> Self {
//         Self {
//             type_: result_type,
//             kind: Instruction::Block { children, result }
//         }
//     }
//     fn int(int: i64) -> Self {
//         Self {
//             type_: Type::Builtin(PrimitiveType::S64),
//             kind: Instruction::Int(int)
//         }
//     }
//     fn float(float: f64) -> Self {
//         Self {
//             type_: Type::Builtin(PrimitiveType::F64),
//             kind: Instruction::Float(float)
//         }
//     }
//     fn string(string: String) -> Self {
//         Self {
//             type_: Type::Builtin(PrimitiveType::String),
//             kind: Instruction::String(string)
//         }
//     }
//     fn bool(b: bool, none: Id) -> Self {
//         Self {
//             type_: Type::Enum(HashSet::from_iter(["True".to_string(), "String".to_string()].into_iter())),
//             kind: Instruction::Enum { variant: if b { "true".to_string() } else { "false".to_string() }, payload: none }
//         }
//     }
//     fn none() -> Self {
//         Self {
//             type_: Type::None,
//             kind: Instruction::None
//         }
//     }
//     fn return_(what: Id) -> Self {
//         Self {
//             type_: Type::Never,
//             kind: Instruction::Return(what)
//         }
//     }
//     fn break_(id: Id) -> Self {
//         Self {
//             type_: Type::Never,
//             kind: Instruction::Break(id)
//         }
//     }
//     fn continue_() -> Self {
//         Self {
//             type_: Type::Never,
//             kind: Instruction::Continue
//         }
//     }
//     fn if_(condition: Id, then: Id, else_: Id) -> Self {
//         // TODO: Join types.
//         Self {
//             type_: Type::None,
//             kind: Instruction::If { condition, then, else_ }
//         }
//     }
//     fn loop_(body: Id) -> Self {
//         // TODO: join types of breaks
//         Self {
//             type_: Type::Never,
//             kind: Instruction::Loop(body),
//         }
//     }
//     fn call(function: String, arguments: Vec<Id>) -> Self {
//         Self {
//             type_: Type::Never,
//             kind: Instruction::FunctionCall { function, arguments }
//         }
//     }
// }

// pub fn get(exprs: &Vec<Expression>, id: Id) -> Expression {
//     exprs[id.0].clone()
// }

impl Display for Module {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "Type:")?;
        for (name, type_) in &self.types {
            writeln!(f, "{name} = {type_}")?;
            write!(f, "\n")?;
        }
        writeln!(f, "Functions:")?;
        for (name, function) in &self.functions {
            write!(f, "Locals:")?;
            for (i, ty) in function.locals.iter().enumerate() {
                write!(f, " {i}: {ty}")?;
            }
            write!(f, "\n")?;
            write!(f, "fun {}", name)?;
            for (name, type_) in &function.parameters {
                write!(f, " {}: ({})", name, type_)?;
            }
            write!(f, " -> {} ", function.return_type)?;
            function.body.fmt(f, 2)?;
            write!(f, "\n\n")?;
        }

        if !self.tests.is_empty() {
            writeln!(f, "Tests:")?;
            for test in &self.tests {
                write!(f, "Locals:")?;
                for (i, ty) in test.locals.iter().enumerate() {
                    write!(f, " {i}: {ty}")?;
                }
                write!(f, "\n")?;
                match &test.name {
                    Some(name) => write!(f, "test \"{name}\" ")?,
                    None => write!(f, "test ")?,
                }
                test.body.fmt(f, 2)?;
                write!(f, "\n\n")?;
            }
        }
        Ok(())
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Never => write!(f, "never")?,
            Type::Void => write!(f, "none")?,
            Type::Blank => write!(f, "blank")?,
            Type::Builtin(builtin) => write!(f, "{}", builtin)?,
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
            Type::Resource(_) => write!(f, "...")?,
            Type::Enum(ty) => {
                let mut first = true;
                for case in &ty.cases {
                    if first {
                        first = false;
                    } else {
                        write!(f, " | ")?;
                    }
                    write!(f, "{case}")?;
                }
            }
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
        };
        Ok(())
    }
}

impl Display for PrimitiveType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name())
    }
}

impl Display for Id {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "@{}", self.0)
    }
}

impl Block {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>, indentation: usize) -> std::fmt::Result {
        let ws = "  ";
        writeln!(f, "{{")?;
        for instruction in &self.instructions {
            for _ in 0..indentation {
                write!(f, "{ws}")?;
            }
            instruction.fmt(f, indentation + 2)?;
            writeln!(f, "")?;
        }
        for _ in 0..indentation - 2 {
            write!(f, "{ws}")?;
        }
        write!(f, "}}")?;
        Ok(())
    }
}

impl Display for LocalId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "@{}", self.0)
    }
}

impl Instruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>, indentation: usize) -> std::fmt::Result {
        match &self {
            Instruction::Pop => write!(f, "pop")?,
            // Instruction::Block { children, result } => {
            //     writeln!(f, "{{")?;
            //     for child in children {
            //         child.fmt(f, indentation + 1, exprs)?;
            //     }
            //     for _ in 0..=indentation {
            //         write!(f, "  ")?;
            //     }
            //     writeln!(f, "{result}")?;
            //     for _ in 0..indentation {
            //         write!(f, "  ")?;
            //     }
            //     write!(f, "}}")?;
            //     return Ok(())
            // },
            Instruction::Int(int) => write!(f, "int {}", int)?,
            Instruction::Float(float) => write!(f, "float {}", float)?,
            Instruction::String(string) => write!(f, "string {:?}", string)?,
            Instruction::Bool(b) => write!(f, "bool {b}")?,
            Instruction::Void => write!(f, "void")?,
            Instruction::Enum { case, ty: _ } => write!(f, "enum case {}", case)?,
            Instruction::Variant { case, num_payloads } => {
                write!(f, "variant case {} with {} payloads", case, num_payloads)?
            }
            Instruction::Record(fields) => {
                write!(f, "record")?;
                for field in fields {
                    write!(f, " {}", field)?;
                }
            }
            Instruction::Option { is_some, ty } => {
                if *is_some {
                    write!(f, "some({ty})")?;
                } else {
                    write!(f, "none")?;
                }
            }
            Instruction::List { len, ty } => {
                write!(f, "list with {} items of {}", len, ty)?;
            }
            Instruction::Call {
                function,
                num_arguments,
            } => {
                write!(f, "call {function} with {num_arguments} arguments")?;
            }
            Instruction::FieldAccess(field) => write!(f, "field access {field}")?,
            Instruction::MemberChain(id, fields) => {
                write!(f, "access {} on {}", fields.join("."), id)?;
            }
            Instruction::IndexAccess { ty: _ } => write!(f, "index access")?,
            Instruction::Load(local) => write!(f, "load from {local}")?,
            Instruction::Store(local) => write!(f, "store to {local}")?,
            Instruction::Return => write!(f, "return")?,
            Instruction::Break => write!(f, "break")?,
            Instruction::Continue => write!(f, "continue")?,
            Instruction::Throw => write!(f, "throw")?,
            Instruction::If { then, else_ } => {
                write!(f, "if ")?;
                then.fmt(f, indentation)?;
                write!(f, " else ")?;
                else_.fmt(f, indentation)?;
            }
            Instruction::Match { arms } => {
                write!(f, "match {{")?;
                for (variant, arm) in arms {
                    write!(f, "  {variant} => ")?;
                    arm.fmt(f, indentation + 2)?;
                }
                write!(f, "}}")?;
            }
            Instruction::Loop(body) => {
                write!(f, "loop ")?;
                body.fmt(f, indentation + 1)?;
            }
            Instruction::Unreachable => write!(f, "unreachable")?,
        }
        Ok(())
    }
}
