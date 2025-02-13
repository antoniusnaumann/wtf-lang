//! The High-Level Intermediate Representation (HIR) is monomorphized: There are
//! no generic functions anymore – instead, the HIR only contains the used
//! functions specialized for concrete types.

mod compiler;
mod type_;
mod visible;

pub use compiler::compile;
use std::{
    collections::HashMap,
    fmt::{write, Display},
};
use type_::Type;

#[derive(Debug, Clone, PartialEq)]
pub struct Module {
    pub types: HashMap<String, Type>,
    pub functions: HashMap<String, Function>,
    pub tests: Vec<Test>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FunctionSignature {
    param_types: Vec<Type>,
    return_type: Type,
    is_export: bool,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Function {
    pub parameters: Vec<Parameter>,
    pub return_type: Type,
    pub body: FunctionBody,
    pub is_export: bool,
}

#[derive(Debug, Clone, PartialEq)]
struct Parameter {
    name: String,
    ty: Type,
}

#[derive(Debug, Clone, PartialEq)]
struct FunctionBody {
    pub expressions: Vec<Expression>,
    pub vars: Vec<Type>,
    pub body: Body,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Test {
    pub name: Option<String>,
    pub id: String,
    pub locals: Vec<Type>, // include parameters
    pub body: FunctionBody,
}

#[derive(Debug, Clone, PartialEq, Copy)]
pub struct VarId(usize);

#[derive(Debug, Clone, PartialEq, Copy)]
pub struct Id(usize);

#[derive(Debug, Clone, PartialEq)]
pub struct Body {
    pub ids: Vec<Id>,
    pub returns: Id,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Expression {
    kind: ExpressionKind,
    ty: Type,
}
#[derive(Debug, Clone, PartialEq)]
pub enum ExpressionKind {
    Parameter(String),
    Reference(Id),
    VarSet {
        var: VarId,
        expression: Id,
    },
    VarGet {
        var: VarId,
    },
    None,
    Int(i64),
    // Float(f64), // TODO: impl
    String(String),
    Bool(bool),
    Enum {
        case: usize,
    },
    Variant {
        case: String,
        payloads: HashMap<String, Id>,
    },
    Record(HashMap<String, Id>),
    Member {
        of: Id,
        name: String,
    },
    List(Vec<Id>),
    IndexAccess {
        of: Id,
        index: Id,
    },
    Tuple(Vec<Id>),
    TupleAccess {
        of: Id,
        index: usize,
    },
    Call {
        function: String,
        arguments: Vec<Id>,
    },
    Unreachable,
    Return(Id),
    Break(Id),
    Continue,
    Throw(Id),
    If {
        condition: Id,
        then: Body,
        else_: Body,
    },
    Match {
        arms: HashMap<String, Body>,
    },
    Loop(Body),
}

impl ExpressionKind {
    fn typed(self, ty: Type) -> Expression {
        Expression { kind: self, ty }
    }
}

impl Expression {
    fn parameter(name: String, ty: Type) -> Self {
        ExpressionKind::Parameter(name).typed(ty)
    }
    fn reference(id: Id, ty: Type) -> Self {
        ExpressionKind::Reference(id).typed(ty)
    }
    fn var_set(var: VarId, expression: Id) -> Expression {
        ExpressionKind::VarSet { var, expression }.typed(Type::None)
    }
    fn var_get(var: VarId, ty: Type) -> Expression {
        ExpressionKind::VarGet { var }.typed(ty)
    }
    fn none() -> Self {
        ExpressionKind::None.typed(Type::None)
    }
    fn int(int: i64) -> Expression {
        ExpressionKind::Int(int).typed(Type::Int {
            signed: true,
            bits: 64,
        })
    }
    // TODO: float
    fn string(string: String) -> Expression {
        ExpressionKind::String(string).typed(Type::String)
    }
    fn bool(_: bool) -> Expression {
        todo!("bool")
    }
    fn variant(case: String, payloads: HashMap<String, Id>, ty: Type) -> Expression {
        ExpressionKind::Variant { case, payloads }.typed(ty)
    }
    fn record(fields: HashMap<String, Id>, ty: Type) -> Expression {
        ExpressionKind::Record(fields).typed(ty)
    }
    fn list(items: Vec<Id>, ty: Type) -> Expression {
        ExpressionKind::List(items).typed(ty)
    }
    fn index_access(of: Id, index: Id, ty: Type) -> Expression {
        ExpressionKind::IndexAccess { of, index }.typed(ty)
    }
    fn tuple(items: Vec<Id>, ty: Type) -> Expression {
        ExpressionKind::Tuple(items).typed(ty)
    }
    fn tuple_access(of: Id, index: usize, ty: Type) -> Expression {
        ExpressionKind::TupleAccess { of, index }.typed(ty)
    }
    fn call(function: String, arguments: Vec<Id>, return_type: Type) -> Expression {
        ExpressionKind::Call {
            function,
            arguments,
        }
        .typed(return_type)
    }
    fn member(of: Id, name: String, ty: Type) -> Expression {
        ExpressionKind::Member { of, name }.typed(ty)
    }
    fn unreachable() -> Expression {
        ExpressionKind::Unreachable.typed(Type::Never)
    }
    fn return_(id: Id) -> Expression {
        ExpressionKind::Return(id).typed(Type::Never)
    }
    fn break_(id: Id) -> Expression {
        ExpressionKind::Break(id).typed(Type::Never)
    }
    fn continue_() -> Expression {
        ExpressionKind::Continue.typed(Type::Never)
    }
    fn throw(id: Id) -> Expression {
        ExpressionKind::Throw(id).typed(Type::Never)
    }
    fn if_(condition: Id, then: Body, else_: Body, ty: Type) -> Expression {
        ExpressionKind::If {
            condition,
            then,
            else_,
        }
        .typed(ty)
    }
    fn match_(arms: HashMap<String, Body>, ty: Type) -> Expression {
        ExpressionKind::Match { arms }.typed(ty)
    }
    fn loop_(body: Body, ty: Type) -> Expression {
        ExpressionKind::Loop(body).typed(ty)
    }
}

impl FunctionBody {
    fn get(&self, id: Id) -> &Expression {
        &self.expressions[id.0]
    }
}

impl Display for Module {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "Type:")?;
        for (name, ty) in &self.types {
            writeln!(f, "{name} = {ty}")?;
            write!(f, "\n")?;
        }
        writeln!(f, "Functions:")?;
        for (name, function) in &self.functions {
            write!(f, "Locals:")?;
            for (i, ty) in function.body.vars.iter().enumerate() {
                write!(f, " {}: {ty}", VarId(i))?;
            }
            write!(f, "\n")?;
            write!(f, "fun {}", name)?;
            for Parameter { name, ty } in &function.parameters {
                write!(f, " {}: {}", name, ty)?;
            }
            write!(f, " -> {} ", function.return_type)?;
            function.body.fmt(f)?;
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
                test.body.fmt(f)?;
                write!(f, "\n\n")?;
            }
        }
        Ok(())
    }
}

impl Display for Id {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "@{}", self.0)
    }
}

impl Display for VarId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "var{}", self.0)
    }
}

impl Display for FunctionBody {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.body.fmt(f, 0, self)
    }
}

impl Body {
    fn fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        indentation: usize,
        fun: &FunctionBody,
    ) -> std::fmt::Result {
        let ws = "  ";
        writeln!(f, "{{")?;
        for id in &self.ids {
            for _ in 0..indentation {
                write!(f, "{ws}")?;
            }
            let expression = &fun.expressions[id.0];
            write!(f, "{}: {} = ", id, expression.ty)?;
            expression.fmt(f, indentation + 2, fun)?;
        }
        for _ in 0..indentation - 2 {
            write!(f, "{ws}")?;
        }
        write!(f, "}}")?;
        Ok(())
    }
}

impl Expression {
    fn fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        indentation: usize,
        fun: &FunctionBody,
    ) -> std::fmt::Result {
        match &self.kind {
            ExpressionKind::Parameter(_) => unreachable!(),
            ExpressionKind::Reference(id) => write!(f, "{}", id)?,
            ExpressionKind::VarSet { var, expression } => write!(f, "{} = {}", var, expression)?,
            ExpressionKind::VarGet { var } => write!(f, "{}", var)?,
            ExpressionKind::Int(int) => write!(f, "int {}", int)?,
            // ExpressionKind::Float(float) => write!(f, "float {}", float)?,
            ExpressionKind::String(string) => write!(f, "string {:?}", string)?,
            ExpressionKind::Bool(b) => write!(f, "bool {b}")?,
            ExpressionKind::None => write!(f, "none")?,
            ExpressionKind::Enum { case } => write!(f, "enum case {}", case)?,
            ExpressionKind::Variant { case, payloads } => {
                write!(f, "variant case {} with payloads:", case)?;
                for (name, value) in payloads {
                    write!(f, " {}: {}", name, value)?;
                }
            }
            ExpressionKind::Record(fields) => {
                write!(f, "record")?;
                for (name, value) in fields {
                    write!(f, " {}: {}", name, value)?;
                }
            }
            ExpressionKind::List(items) => {
                write!(f, "list with items:")?;
                for item in items {
                    write!(f, " {}", item)?;
                }
            }
            ExpressionKind::IndexAccess { of, index } => write!(f, "{of}[{index}]")?,
            ExpressionKind::Tuple(tuple) => {
                write!(f, "(")?;
                for id in tuple {
                    write!(f, "{}", id)?;
                }
                write!(f, ")")?;
            }
            ExpressionKind::TupleAccess { of, index } => write!(f, "{of}.{index}")?,
            ExpressionKind::Call {
                function,
                arguments,
            } => {
                write!(f, "call {function} with arguments:")?;
                for arg in arguments {
                    write!(f, "{}", arg)?;
                }
            }
            ExpressionKind::Member { of, name } => write!(f, "{of}.{name}")?,
            ExpressionKind::Unreachable => write!(f, "unreachable")?,
            ExpressionKind::Return(id) => write!(f, "return {id}")?,
            ExpressionKind::Break(id) => write!(f, "break {id}")?,
            ExpressionKind::Continue => write!(f, "continue")?,
            ExpressionKind::Throw(id) => write!(f, "throw {id}")?,
            ExpressionKind::If {
                condition,
                then,
                else_,
            } => {
                write!(f, "if {condition}")?;
                then.fmt(f, indentation, fun)?;
                write!(f, " else ")?;
                else_.fmt(f, indentation, fun)?;
            }
            ExpressionKind::Match { arms } => {
                write!(f, "match {{")?;
                for (variant, arm) in arms {
                    write!(f, "  {variant} => ")?;
                    arm.fmt(f, indentation + 2, fun)?;
                }
                write!(f, "}}")?;
            }
            ExpressionKind::Loop(body) => {
                write!(f, "loop ")?;
                body.fmt(f, indentation + 1, fun)?;
            }
        }
        Ok(())
    }
}
