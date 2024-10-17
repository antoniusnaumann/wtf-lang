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

impl Print for [String] {
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

impl Print for [Declaration] {
    fn print(&self, f: &mut std::fmt::Formatter<'_>, indent: usize, c: char) -> std::fmt::Result {
        for elem in self {
            node!(f, indent, c, "decl", elem)?;
        }

        Ok(())
    }
}

impl Print for Declaration {
    fn print(&self, f: &mut std::fmt::Formatter<'_>, indent: usize, c: char) -> std::fmt::Result {
        match self {
            Declaration::Function(v) => v.print(f, indent, c),
            Declaration::Record(v) => v.print(f, indent, c),
            Declaration::Resource(v) => v.print(f, indent, c),
            Declaration::Enum(v) => v.print(f, indent, c),
            Declaration::Variant(v) => v.print(f, indent, c),
            Declaration::Export(v) => v.print(f, indent, c),
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

impl Print for RecordDeclaration {
    fn print(&self, f: &mut std::fmt::Formatter<'_>, indent: usize, c: char) -> std::fmt::Result {
        node!(f, indent, c, "record", self.name, self.fields)
    }
}

impl Print for [Field] {
    fn print(&self, f: &mut std::fmt::Formatter<'_>, indent: usize, c: char) -> std::fmt::Result {
        for field in self {
            field.print(f, indent, c)?;
        }

        Ok(())
    }
}

impl Print for Field {
    fn print(&self, f: &mut std::fmt::Formatter<'_>, indent: usize, c: char) -> std::fmt::Result {
        node!(f, indent, c, "field", self.name, self.type_annotation)
    }
}

impl Print for ResourceDeclaration {
    fn print(&self, f: &mut std::fmt::Formatter<'_>, indent: usize, c: char) -> std::fmt::Result {
        todo!()
    }
}

impl Print for EnumDeclaration {
    fn print(&self, f: &mut std::fmt::Formatter<'_>, indent: usize, c: char) -> std::fmt::Result {
        todo!()
    }
}

impl Print for VariantDeclaration {
    fn print(&self, f: &mut std::fmt::Formatter<'_>, indent: usize, c: char) -> std::fmt::Result {
        todo!()
    }
}

impl Print for ExportDeclaration {
    fn print(&self, f: &mut std::fmt::Formatter<'_>, indent: usize, c: char) -> std::fmt::Result {
        node!(f, indent, c, "export", self.item)
    }
}

impl Print for [Parameter] {
    fn print(&self, f: &mut std::fmt::Formatter<'_>, indent: usize, c: char) -> std::fmt::Result {
        for elem in self {
            elem.print(f, indent, c)?;
        }

        Ok(())
    }
}

impl Print for Parameter {
    fn print(&self, f: &mut std::fmt::Formatter<'_>, indent: usize, c: char) -> std::fmt::Result {
        node!(f, indent, c, "param", self.name, self.type_annotation)
    }
}

impl Print for TypeAnnotation {
    fn print(&self, f: &mut std::fmt::Formatter<'_>, indent: usize, c: char) -> std::fmt::Result {
        match self {
            TypeAnnotation::Simple(v) => v.print(f, indent, c),
            TypeAnnotation::List(elem) => node!(f, indent, c, "list", elem),
            TypeAnnotation::Option(inner) => {
                node!(f, indent, c, "option", inner)
            }
            TypeAnnotation::Result { ok, err } => {
                node!(f, indent, c, "result", ok, err)
            }
            TypeAnnotation::Tuple(members) => {
                todo!()
            }
        }
    }
}

impl Print for Block {
    fn print(&self, f: &mut std::fmt::Formatter<'_>, indent: usize, c: char) -> std::fmt::Result {
        node!(f, indent, c, "block", self.statements)
    }
}

impl Print for [Statement] {
    fn print(&self, f: &mut std::fmt::Formatter<'_>, indent: usize, c: char) -> std::fmt::Result {
        for elem in self {
            elem.print(f, indent, c)?;
        }

        Ok(())
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
            Statement::WhileStatement(_) => todo!(),
            Statement::ForStatement(_) => todo!(),
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

impl Print for Expression {
    fn print(&self, f: &mut std::fmt::Formatter<'_>, indent: usize, c: char) -> std::fmt::Result {
        match self {
            Expression::Literal(v) => v.print(f, indent, c),
            Expression::Identifier(v) => write!(f, "\n{c:indent$}(ident {v})"),
            Expression::BinaryExpression {
                left,
                operator,
                right,
            } => {
                node!(f, indent, c, "binary", left, operator, right)
            }
            Expression::UnaryExpression { operator, operand } => {
                node!(f, indent, c, "unary", operator, operand)
            }
            Expression::YeetExpression { expression } => todo!(),
            Expression::FunctionCall {
                function,
                arguments,
            } => {
                node!(f, indent, c, "call", function, Args(arguments))
            }
            Expression::MethodCall {
                receiver,
                method,
                arguments,
                safe,
            } => todo!(),
            Expression::FieldAccess {
                object,
                field,
                safe,
            } => todo!(),
            Expression::IndexAccess { collection, index } => todo!(),
            Expression::Record { name, members } => todo!(),
            Expression::ListLiteral(_) => todo!(),
        }
    }
}

struct Args<'a>(&'a [Expression]);
impl Print for Args<'_> {
    fn print(&self, f: &mut std::fmt::Formatter<'_>, indent: usize, c: char) -> std::fmt::Result {
        node!(f, indent, c, "args", self.0)
    }
}

impl Print for [Expression] {
    fn print(&self, f: &mut std::fmt::Formatter<'_>, indent: usize, c: char) -> std::fmt::Result {
        for elem in self {
            elem.print(f, indent, c)?;
        }

        Ok(())
    }
}

impl Print for Literal {
    fn print(&self, f: &mut std::fmt::Formatter<'_>, indent: usize, c: char) -> std::fmt::Result {
        match self {
            Literal::Integer(v) => write!(f, "\n{c:indent$}(int {v})"),
            Literal::Float(v) => write!(f, "\n{c:indent$}(float {v})"),
            Literal::String(v) => write!(f, "\n{c:indent$}(string \"{v}\")"),
            Literal::Boolean(v) => write!(f, "\n{c:indent$}(bool {v})"),
            Literal::None => write!(f, "\n{c:indent$}(none)"),
        }
    }
}

impl Print for BinaryOperator {
    fn print(&self, f: &mut std::fmt::Formatter<'_>, indent: usize, c: char) -> std::fmt::Result {
        let op = match self {
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
