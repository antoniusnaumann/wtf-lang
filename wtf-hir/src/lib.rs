//! The High-Level Intermediate Representation (HIR) is monomorphized: There are
//! no generic functions anymore – instead, the HIR only contains the used
//! functions specialized for concrete types.

mod builtin;
mod compiler;
mod type_;
mod visible;

pub use compiler::compile;
use std::borrow::Cow;
use std::ops::Index;
use std::{collections::HashMap, fmt::Display};
pub use type_::Type;

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
pub struct Parameter {
    pub name: String,
    pub ty: Type,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionBody {
    pub vars: Vec<Type>,
    pub body: Body,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Test {
    pub name: Option<String>,
    pub id: String,
    pub body: FunctionBody,
}

#[derive(Debug, Clone, PartialEq, Copy)]
pub struct VarId(pub usize);

#[derive(Debug, Clone, PartialEq)]
pub struct Body {
    pub statements: Vec<Expression>,
}

impl Body {
    pub fn returns(&self) -> Cow<'_, Expression> {
        self.statements
            .last()
            .map(Cow::Borrowed)
            .unwrap_or_else(|| Cow::Owned(Expression::void()))
    }
}

impl From<Expression> for Body {
    fn from(value: Expression) -> Self {
        Body {
            statements: vec![value],
        }
    }
}

impl<T> From<T> for Body
where
    T: Into<Vec<Expression>>,
{
    fn from(value: T) -> Self {
        Body {
            statements: value.into(),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Expression {
    pub kind: ExpressionKind,
    pub ty: Type,
}
#[derive(Debug, Clone, PartialEq)]
pub enum ExpressionKind {
    Parameter(String),
    Reference(Box<Expression>),
    VarSet {
        var: VarId,
        expression: Box<Expression>,
    },
    VarGet {
        var: VarId,
    },
    /// Our 'void' type that can be implicitly casted to an empty optional
    None,
    /// The zero value of a type, only used internally, e.g. to allow uninitialized variables
    Zero,
    Int(i64),
    Float(f64),
    String(String),
    Bool(bool),
    Enum {
        case: usize,
    },
    Variant {
        case: String,
        payloads: Vec<(String, Expression)>,
    },
    Record(Vec<(String, Expression)>),
    Member {
        of: Box<Expression>,
        name: String,
    },
    Option(Option<Box<Expression>>),
    List(Vec<Expression>),
    IndexAccess {
        of: Box<Expression>,
        index: Box<Expression>,
    },
    Tuple(Vec<Expression>),
    TupleAccess {
        of: Box<Expression>,
        index: usize,
    },
    Call {
        function: String,
        arguments: Vec<Expression>,
    },
    Unreachable,
    Return(Box<Expression>),
    Break(Box<Expression>),
    Continue,
    Throw(Box<Expression>),
    If {
        condition: Box<Expression>,
        then: Body,
        else_: Body,
    },
    Match {
        arms: HashMap<String, Body>,
    },
    Loop(Body),
    Type(Type),
    Multiple(Vec<Expression>),
}

impl ExpressionKind {
    pub const fn typed(self, ty: Type) -> Expression {
        Expression { kind: self, ty }
    }
}

impl Index<VarId> for Vec<Type> {
    type Output = Type;

    fn index(&self, index: VarId) -> &Self::Output {
        &self[index.0]
    }
}

impl Index<VarId> for [Type] {
    type Output = Type;

    fn index(&self, index: VarId) -> &Self::Output {
        &self[index.0]
    }
}

impl Expression {
    fn parameter(name: String, ty: Type) -> Self {
        ExpressionKind::Parameter(name).typed(ty)
    }
    fn reference(expression: Expression, ty: Type) -> Self {
        ExpressionKind::Reference(expression.into()).typed(ty)
    }
    fn var_set(var: VarId, expression: Expression) -> Expression {
        ExpressionKind::VarSet {
            var,
            expression: expression.into(),
        }
        .typed(Type::None)
    }
    fn var_get(var: VarId, ty: Type) -> Expression {
        ExpressionKind::VarGet { var }.typed(ty)
    }
    const fn void() -> Self {
        ExpressionKind::None.typed(Type::None)
    }
    fn int(int: i64) -> Expression {
        ExpressionKind::Int(int).typed(Type::Int {
            signed: true,
            bits: 32,
        })
    }
    fn float(float: f64) -> Expression {
        ExpressionKind::Float(float).typed(Type::Float { bits: 32 })
    }
    fn string(string: String) -> Expression {
        ExpressionKind::String(string).typed(Type::String)
    }
    fn bool(value: bool) -> Expression {
        ExpressionKind::Bool(value).typed(Type::Bool)
    }
    fn variant(case: String, payloads: Vec<(String, Expression)>, ty: Type) -> Expression {
        ExpressionKind::Variant { case, payloads }.typed(ty)
    }
    fn record(fields: Vec<(String, Expression)>, ty: Type) -> Expression {
        ExpressionKind::Record(fields).typed(ty)
    }
    fn some(inner: Expression) -> Expression {
        let ty = inner.ty.clone();
        ExpressionKind::Option(Some(Box::new(inner))).typed(Type::Option(ty.into()))
    }
    fn none(inner_type: Type) -> Expression {
        ExpressionKind::Option(None).typed(Type::Option(inner_type.into()))
    }
    fn list(items: Vec<Expression>, ty: Type) -> Expression {
        ExpressionKind::List(items).typed(ty)
    }
    fn index_access(of: Expression, index: Expression, ty: Type) -> Expression {
        ExpressionKind::IndexAccess {
            of: of.into(),
            index: index.into(),
        }
        .typed(ty)
    }
    fn tuple(items: Vec<Expression>, ty: Type) -> Expression {
        ExpressionKind::Tuple(items).typed(ty)
    }
    fn tuple_access(of: Expression, index: usize, ty: Type) -> Expression {
        ExpressionKind::TupleAccess {
            of: of.into(),
            index,
        }
        .typed(ty)
    }
    fn call(function: String, arguments: Vec<Expression>, return_type: Type) -> Expression {
        ExpressionKind::Call {
            function,
            arguments,
        }
        .typed(return_type)
    }
    fn member(of: Expression, name: String, ty: Type) -> Expression {
        ExpressionKind::Member {
            of: of.into(),
            name,
        }
        .typed(ty)
    }
    fn unreachable() -> Expression {
        ExpressionKind::Unreachable.typed(Type::Never)
    }
    fn return_(id: Expression) -> Expression {
        ExpressionKind::Return(id.into()).typed(Type::Never)
    }
    fn break_(id: Expression) -> Expression {
        ExpressionKind::Break(id.into()).typed(Type::Never)
    }
    fn continue_() -> Expression {
        ExpressionKind::Continue.typed(Type::Never)
    }
    fn throw(id: Expression) -> Expression {
        ExpressionKind::Throw(id.into()).typed(Type::Never)
    }
    fn if_(condition: Expression, then: Body, else_: Body, ty: Type) -> Expression {
        ExpressionKind::If {
            condition: condition.into(),
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
    fn multiple(exprs: impl Into<Vec<Expression>>) -> Expression {
        let exprs = exprs.into();
        let ty = exprs
            .iter()
            .last()
            .map(|e| e.ty.clone())
            .unwrap_or(Type::None);
        ExpressionKind::Multiple(exprs).typed(ty.clone())
    }
}

impl Display for Module {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for (name, ty) in &self.types {
            writeln!(f, "{name} = {ty}")?;
        }
        for (name, function) in &self.functions {
            write!(f, "fun {}", name)?;
            for Parameter { name, ty } in &function.parameters {
                write!(f, " {}: {}", name, ty)?;
            }
            writeln!(f, " -> {} {{", function.return_type)?;
            for (i, ty) in function.body.vars.iter().enumerate() {
                write!(f, "  {}: {ty}", VarId(i))?;
                if let Some(param) = function.parameters.get(i) {
                    write!(f, " (param {})", param.name)?;
                }
                writeln!(f, "")?;
            }
            function.body.fmt(f)?;
            write!(f, "\n}}\n")?;
        }

        if !self.tests.is_empty() {
            writeln!(f, "Tests:")?;
            for test in &self.tests {
                write!(f, "Locals:")?;
                for (i, ty) in test.body.vars.iter().enumerate() {
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

impl Display for VarId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "var{}", self.0)
    }
}

impl Display for FunctionBody {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.body.fmt(f, 1, self)
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
        for _ in 0..indentation {
            write!(f, "{ws}")?;
        }
        writeln!(f, "{{")?;
        for statement in &self.statements {
            for _ in 0..(indentation + 1) {
                write!(f, "{ws}")?;
            }
            statement.fmt(f, indentation + 1, fun)?;
            writeln!(f, "")?;
        }
        for _ in 0..indentation {
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
            ExpressionKind::Reference(expression) => expression.fmt(f, indentation, fun)?,
            ExpressionKind::VarSet { var, expression } => {
                write!(f, "{} = ", var)?;
                expression.fmt(f, indentation, fun)?
            }
            ExpressionKind::VarGet { var } => write!(f, "{}", var)?,
            ExpressionKind::Int(int) => write!(f, "int {}", int)?,
            ExpressionKind::Float(float) => write!(f, "float {}", float)?,
            ExpressionKind::String(string) => write!(f, "string {:?}", string)?,
            ExpressionKind::Bool(b) => write!(f, "bool {b}")?,
            ExpressionKind::None => write!(f, "none")?,
            ExpressionKind::Zero => write!(f, "zero")?,
            ExpressionKind::Enum { case } => write!(f, "enum case {}", case)?,
            ExpressionKind::Variant { case, payloads } => {
                write!(f, "variant case {} with payloads:", case)?;
                for (name, value) in payloads {
                    write!(f, " {}: ", name)?;
                    value.fmt(f, indentation, fun)?
                }
            }
            ExpressionKind::Record(fields) => {
                write!(f, "record")?;
                for (name, value) in fields {
                    write!(f, " {}: ", name)?;
                    value.fmt(f, indentation, fun)?
                }
            }
            ExpressionKind::Option(option) => match option {
                Some(some) => {
                    write!(f, "some(")?;
                    some.fmt(f, indentation, fun)?;
                    write!(f, ")")?;
                }
                None => write!(f, "none")?,
            },
            ExpressionKind::List(items) => {
                write!(f, "list with items:")?;
                for item in items {
                    write!(f, " ")?;
                    item.fmt(f, indentation, fun)?
                }
            }
            ExpressionKind::IndexAccess { of, index } => {
                of.fmt(f, indentation, fun)?;
                write!(f, "[")?;
                index.fmt(f, indentation, fun)?;
                write!(f, "]")?;
            }
            ExpressionKind::Tuple(tuple) => {
                write!(f, "(")?;
                for expr in tuple {
                    expr.fmt(f, indentation, fun)?;
                }
                write!(f, ")")?;
            }
            ExpressionKind::TupleAccess { of, index } => {
                of.fmt(f, indentation, fun)?;
                write!(f, ".{index}")?
            }
            ExpressionKind::Call {
                function,
                arguments,
            } => {
                write!(f, "call {function} with arguments:")?;
                for arg in arguments {
                    write!(f, " ")?;
                    arg.fmt(f, indentation, fun)?
                }
            }
            ExpressionKind::Member { of, name } => {
                of.fmt(f, indentation, fun)?;
                write!(f, ".{name}")?
            }
            ExpressionKind::Unreachable => write!(f, "unreachable")?,
            ExpressionKind::Return(expr) => {
                write!(f, "return ")?;
                expr.fmt(f, indentation, fun)?
            }
            ExpressionKind::Break(expr) => {
                write!(f, "break ")?;
                expr.fmt(f, indentation, fun)?
            }
            ExpressionKind::Continue => write!(f, "continue")?,
            ExpressionKind::Throw(expr) => {
                write!(f, "throw ")?;
                expr.fmt(f, indentation, fun)?
            }
            ExpressionKind::If {
                condition,
                then,
                else_,
            } => {
                write!(f, "if ")?;
                condition.fmt(f, indentation, fun)?;
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
            ExpressionKind::Type(ty) => {
                write!(f, "type {ty}")?;
            }
            ExpressionKind::Multiple(exprs) => {
                write!(f, "multi(")?;
                for e in exprs {
                    e.fmt(f, indentation, fun)?;
                    write!(f, " ")?;
                }
                write!(f, ")")?;
            }
        }
        Ok(())
    }
}
