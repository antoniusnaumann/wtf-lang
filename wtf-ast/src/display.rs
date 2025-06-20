use std::fmt::Display;

use crate::*;

impl Display for Module {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "(module")?;

        self.package.print(f, 2, ' ')?;
        self.uses.print(f, 2, ' ')?;
        self.declarations.print(f, 2, ' ')?;

        write!(f, ")")
    }
}

trait Print {
    fn print(&self, f: &mut std::fmt::Formatter<'_>, indent: usize, c: char) -> std::fmt::Result;
}

impl<P: Print> Print for Option<P> {
    fn print(&self, f: &mut std::fmt::Formatter<'_>, indent: usize, c: char) -> std::fmt::Result {
        if let Some(inner) = self {
            inner.print(f, indent, c)
        } else {
            Ok(())
        }
    }
}

macro_rules! node {
    ($f:expr, $indent:expr, $c:expr, $name:expr, $( $args:expr ),* ) => {
        {
            write!($f, "\n{c:indent$}({}", $name, c = $c, indent = $indent)?;
            $( $args.print($f, $indent + 2, $c)?);*;
            write!($f, ")")
        }
    }
}

impl Print for PackageDeclaration {
    fn print(&self, f: &mut std::fmt::Formatter<'_>, indent: usize, c: char) -> std::fmt::Result {
        node!(f, indent, c, "package", self.path, self.version)
    }
}

impl Print for ModulePath {
    fn print(&self, f: &mut std::fmt::Formatter<'_>, indent: usize, c: char) -> std::fmt::Result {
        node!(f, indent, c, "path", self.owner, self.package)
    }
}

impl Print for Version {
    fn print(&self, f: &mut std::fmt::Formatter<'_>, indent: usize, c: char) -> std::fmt::Result {
        write!(
            f,
            "\n{c:indent$}(version {} {} {})",
            self.major, self.minor, self.patch
        )
    }
}

impl Print for String {
    fn print(&self, f: &mut std::fmt::Formatter<'_>, _indent: usize, _c: char) -> std::fmt::Result {
        write!(f, " \"{self}\"")
    }
}

impl Print for str {
    fn print(&self, f: &mut std::fmt::Formatter<'_>, _indent: usize, _c: char) -> std::fmt::Result {
        write!(f, " \"{self}\"")
    }
}

impl Print for Vec<String> {
    fn print(&self, f: &mut std::fmt::Formatter<'_>, indent: usize, c: char) -> std::fmt::Result {
        let strings = self.join(" ");
        write!(f, "\n{c:indent$}({})", strings)
    }
}

impl Print for [UseDeclaration] {
    fn print(&self, f: &mut std::fmt::Formatter<'_>, indent: usize, c: char) -> std::fmt::Result {
        for elem in self {
            node!(
                f,
                indent,
                c,
                "use",
                elem.module_path,
                elem.interface,
                elem.types
            )?;
        }

        Ok(())
    }
}

impl Print for Declaration {
    fn print(&self, f: &mut std::fmt::Formatter<'_>, indent: usize, c: char) -> std::fmt::Result {
        match self {
            Declaration::Function(v) => v.print(f, indent, c),
            Declaration::Overload(v) => v.print(f, indent, c),
            Declaration::Record(v) => v.print(f, indent, c),
            Declaration::Resource(v) => v.print(f, indent, c),
            Declaration::Enum(v) => v.print(f, indent, c),
            Declaration::Variant(v) => v.print(f, indent, c),
            Declaration::Export(v) => v.print(f, indent, c),
            Declaration::Test(v) => v.print(f, indent, c),
        }
    }
}

impl Print for FunctionDeclaration {
    fn print(&self, f: &mut std::fmt::Formatter<'_>, indent: usize, c: char) -> std::fmt::Result {
        node!(
            f,
            indent,
            c,
            "func",
            self.name,
            self.parameters,
            self.return_type,
            self.body
        )
    }
}

impl Print for OverloadDeclaration {
    fn print(&self, f: &mut std::fmt::Formatter<'_>, indent: usize, c: char) -> std::fmt::Result {
        node!(f, indent, c, "overload", self.name, self.overloads)
    }
}

impl Print for RecordDeclaration {
    fn print(&self, f: &mut std::fmt::Formatter<'_>, indent: usize, c: char) -> std::fmt::Result {
        node!(f, indent, c, "record", self.name, self.fields)
    }
}

impl Print for Field {
    fn print(&self, f: &mut std::fmt::Formatter<'_>, indent: usize, c: char) -> std::fmt::Result {
        node!(f, indent, c, "field", self.name, self.type_annotation)
    }
}

impl Print for ResourceDeclaration {
    fn print(&self, f: &mut std::fmt::Formatter<'_>, indent: usize, c: char) -> std::fmt::Result {
        node!(
            f,
            indent,
            c,
            "resource",
            self.name,
            self.fields,
            self.methods
        )
    }
}

struct Case<'a>(&'a str);

impl Print for EnumDeclaration {
    fn print(&self, f: &mut std::fmt::Formatter<'_>, indent: usize, c: char) -> std::fmt::Result {
        node!(
            f,
            indent,
            c,
            "enum",
            self.name,
            self.cases.iter().map(|s| Case(s)).collect::<Vec<_>>()
        )
    }
}

impl Print for Case<'_> {
    fn print(&self, f: &mut std::fmt::Formatter<'_>, indent: usize, c: char) -> std::fmt::Result {
        node!(f, indent, c, "case", self.0)
    }
}

impl Print for VariantDeclaration {
    fn print(&self, f: &mut std::fmt::Formatter<'_>, indent: usize, c: char) -> std::fmt::Result {
        node!(f, indent, c, "variant", self.cases)
    }
}

impl Print for VariantCase {
    fn print(&self, f: &mut std::fmt::Formatter<'_>, indent: usize, c: char) -> std::fmt::Result {
        node!(f, indent, c, "case", self.name, self.associated_types)
    }
}

impl Print for ExportDeclaration {
    fn print(&self, f: &mut std::fmt::Formatter<'_>, indent: usize, c: char) -> std::fmt::Result {
        node!(f, indent, c, "export", self.item)
    }
}

impl Print for TestDeclaration {
    fn print(&self, f: &mut std::fmt::Formatter<'_>, indent: usize, c: char) -> std::fmt::Result {
        node!(f, indent, c, "test", self.name, self.body)
    }
}

impl Print for Parameter {
    fn print(&self, f: &mut std::fmt::Formatter<'_>, indent: usize, c: char) -> std::fmt::Result {
        node!(f, indent, c, "param", self.name, self.type_annotation)
    }
}

impl Print for TypeAnnotationKind {
    fn print(&self, f: &mut std::fmt::Formatter<'_>, indent: usize, c: char) -> std::fmt::Result {
        match self {
            TypeAnnotationKind::Simple(v) => v.print(f, indent, c),
            TypeAnnotationKind::List(elem) => node!(f, indent, c, "list", elem),
            TypeAnnotationKind::Option(inner) => {
                node!(f, indent, c, "option", inner)
            }
            TypeAnnotationKind::Result { ok, err } => {
                node!(f, indent, c, "result", ok, err)
            }
            TypeAnnotationKind::Tuple(members) => {
                todo!()
            }
        }
    }
}

impl Print for TypeAnnotation {
    fn print(&self, f: &mut std::fmt::Formatter<'_>, indent: usize, c: char) -> std::fmt::Result {
        self.kind.print(f, indent, c)
    }
}

impl Print for Block {
    fn print(&self, f: &mut std::fmt::Formatter<'_>, indent: usize, c: char) -> std::fmt::Result {
        node!(f, indent, c, "block", self.statements)
    }
}

impl Print for Statement {
    fn print(&self, f: &mut std::fmt::Formatter<'_>, indent: usize, c: char) -> std::fmt::Result {
        match self {
            Statement::VariableDeclaration(v) => v.print(f, indent, c),
            Statement::Assignment { target, value } => node!(f, indent, c, "assign", target, value),
            Statement::ExpressionStatement(v) => v.print(f, indent, c),
            Statement::ReturnStatement(v) => node!(f, indent, c, "return", v),
            Statement::BreakStatement(v) => node!(f, indent, c, "break", v),
            Statement::ContinueStatement => write!(f, "\n{c:indent$}(continue)"),
            Statement::ThrowStatement(v) => node!(f, indent, c, "throw", v),
            Statement::IfStatement(v) => v.print(f, indent, c),
            Statement::MatchStatement(_) => todo!(),
            Statement::WhileStatement(v) => v.print(f, indent, c),
            Statement::ForStatement(v) => v.print(f, indent, c),
            Statement::Assertion(v) => v.print(f, indent, c),
            Statement::EmptyLine => write!(f, "\n{c:indent$}(empty)"),
        }
    }
}

impl Print for VariableDeclaration {
    fn print(&self, f: &mut std::fmt::Formatter<'_>, indent: usize, c: char) -> std::fmt::Result {
        node!(
            f,
            indent,
            c,
            if self.mutable { "var" } else { "let" },
            self.name,
            self.type_annotation,
            self.value
        )
    }
}

impl Print for IfStatement {
    fn print(&self, f: &mut std::fmt::Formatter<'_>, indent: usize, c: char) -> std::fmt::Result {
        node!(
            f,
            indent,
            c,
            "if",
            self.condition,
            self.then_branch,
            self.else_branch
        )
    }
}

impl Print for WhileStatement {
    fn print(&self, f: &mut std::fmt::Formatter<'_>, indent: usize, c: char) -> std::fmt::Result {
        node!(f, indent, c, "while", self.condition, self.body)
    }
}

impl Print for ForStatement {
    fn print(&self, f: &mut std::fmt::Formatter<'_>, indent: usize, c: char) -> std::fmt::Result {
        node!(f, indent, c, "for", self.variable, self.iterable, self.body)
    }
}

impl Print for AssertStatement {
    fn print(&self, f: &mut std::fmt::Formatter<'_>, indent: usize, c: char) -> std::fmt::Result {
        node!(f, indent, c, "assert", self.condition)
    }
}

impl Print for Expression {
    fn print(&self, f: &mut std::fmt::Formatter<'_>, indent: usize, c: char) -> std::fmt::Result {
        match &self.kind {
            ExpressionKind::Literal(v) => v.print(f, indent, c),
            ExpressionKind::Identifier(v) => write!(f, "\n{c:indent$}(ident {v})"),
            ExpressionKind::BinaryExpression {
                left,
                operator,
                right,
            } => {
                node!(f, indent, c, "binary", left, operator, right)
            }
            ExpressionKind::UnaryExpression { operator, operand } => {
                node!(f, indent, c, "unary", operator, operand)
            }
            ExpressionKind::YeetExpression { expression } => todo!(),
            ExpressionKind::FunctionCall {
                function,
                arguments,
            } => {
                node!(f, indent, c, "call", function, Args(arguments))
            }
            ExpressionKind::MethodCall {
                receiver,
                method,
                arguments,
                safe,
            } => {
                if *safe {
                    node!(
                        f,
                        indent,
                        c,
                        "safecall",
                        method,
                        Receiver(receiver),
                        Args(arguments)
                    )
                } else {
                    node!(
                        f,
                        indent,
                        c,
                        "call",
                        method,
                        Receiver(receiver),
                        Args(arguments)
                    )
                }
            }
            ExpressionKind::FieldAccess {
                object,
                field,
                safe,
            } => {
                if *safe {
                    node!(f, indent, c, "safeaccess", object, field)
                } else {
                    node!(f, indent, c, "access", object, field)
                }
            }
            ExpressionKind::IndexAccess { collection, index } => {
                node!(f, indent, c, "indexaccess", collection, index)
            }
            ExpressionKind::Record { name, members } => {
                node!(f, indent, c, "record", name, members)
            }
            ExpressionKind::ListLiteral(elements) => node!(f, indent, c, "list", elements),
        }
    }
}

struct Args<'a>(&'a [Expression]);
impl Print for Args<'_> {
    fn print(&self, f: &mut std::fmt::Formatter<'_>, indent: usize, c: char) -> std::fmt::Result {
        node!(f, indent, c, "args", self.0)
    }
}

struct Receiver<'a>(&'a Expression);
impl Print for Receiver<'_> {
    fn print(&self, f: &mut std::fmt::Formatter<'_>, indent: usize, c: char) -> std::fmt::Result {
        node!(f, indent, c, "receiver", self.0)
    }
}

impl Print for Literal {
    fn print(&self, f: &mut std::fmt::Formatter<'_>, indent: usize, c: char) -> std::fmt::Result {
        match &self.kind {
            LiteralKind::Integer(v) => write!(f, "\n{c:indent$}(int {v})"),
            LiteralKind::Float(v) => write!(f, "\n{c:indent$}(float {v})"),
            LiteralKind::String(v) => write!(f, "\n{c:indent$}(string \"{v}\")"),
            LiteralKind::Boolean(v) => write!(f, "\n{c:indent$}(bool {v})"),
            LiteralKind::None => write!(f, "\n{c:indent$}(none)"),
        }
    }
}

impl Print for BinaryOperator {
    fn print(&self, f: &mut std::fmt::Formatter<'_>, indent: usize, c: char) -> std::fmt::Result {
        let op = match self {
            BinaryOperator::Logic(inner) => match inner {
                LogicOperator::And => "and",
                LogicOperator::Or => "or",
            },
            BinaryOperator::Arithmetic(inner) => match inner {
                ArithmeticOperator::Add => "+",
                ArithmeticOperator::Subtract => "-",
                ArithmeticOperator::Multiply => "*",
                ArithmeticOperator::Divide => "/",
            },
            BinaryOperator::Equal => "==",
            BinaryOperator::NotEqual => "!=",
            BinaryOperator::GreaterThan => ">",
            BinaryOperator::LessThan => "<",
            BinaryOperator::GreaterEqual => ">=",
            BinaryOperator::LessEqual => "<=",
            BinaryOperator::Contains => "in",
            BinaryOperator::NullCoalesce => "?",
        };

        write!(f, "\n{c:indent$}(op {op})")
    }
}

impl Print for UnaryOperator {
    fn print(&self, f: &mut std::fmt::Formatter<'_>, indent: usize, c: char) -> std::fmt::Result {
        let op = match self {
            UnaryOperator::Negate => "-",
            UnaryOperator::Not => "!",
        };

        write!(f, "\n{c:indent$}(op {op})")
    }
}

impl Print for FieldAssignment {
    fn print(&self, f: &mut std::fmt::Formatter<'_>, indent: usize, c: char) -> std::fmt::Result {
        node!(f, indent, c, "fieldinit", self.name, self.element)
    }
}

impl<T: Print> Print for [T] {
    fn print(&self, f: &mut std::fmt::Formatter<'_>, indent: usize, c: char) -> std::fmt::Result {
        for elem in self {
            elem.print(f, indent, c)?;
        }

        Ok(())
    }
}
