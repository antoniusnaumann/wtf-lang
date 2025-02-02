use std::borrow::Cow;

use wtf_ast::{
    ArithmeticOperator, BinaryOperator, Block, Declaration, EnumDeclaration, ExportDeclaration,
    Expression, Field, FunctionDeclaration, IfStatement, Literal, Module, ModulePath,
    PackageDeclaration, Parameter, RecordDeclaration, ResourceDeclaration, Statement,
    TestDeclaration, TypeAnnotation, UnaryOperator, UseDeclaration, VariantCase,
    VariantDeclaration, Version, WhileStatement,
};

pub trait FormatPrint {
    fn format_print(&self, indent: usize) -> String;
}

impl FormatPrint for Module {
    fn format_print(&self, indent: usize) -> String {
        let package = self.package.format_print(indent);
        let uses = self.uses.format_print("\n\n", indent);
        let declarations = self.declarations.format_print("\n\n", indent);

        format!(
            "{package}{}{uses}{}{declarations}",
            if package.is_empty() { "" } else { "\n\n" },
            if uses.is_empty() { "" } else { "\n\n" }
        )
    }
}

impl FormatPrint for PackageDeclaration {
    fn format_print(&self, indent: usize) -> String {
        let path = self.path.format_print(0);
        let version = self.version.format_print(0);

        format!(
            "{}{path}{}{version}",
            tab(indent),
            if version.is_empty() { "" } else { "@" }
        )
    }
}

impl FormatPrint for ModulePath {
    fn format_print(&self, _indent: usize) -> String {
        format!("{}:{}", self.owner, self.package)
    }
}

impl FormatPrint for Version {
    fn format_print(&self, _indent: usize) -> String {
        format!("{}.{}.{}", self.major, self.minor, self.patch)
    }
}

impl FormatPrint for UseDeclaration {
    fn format_print(&self, indent: usize) -> String {
        let path = self.module_path.format_print(indent);
        let types = self.types.join(", ");

        format!("{path}/{}.{{{types}}}", self.interface)
    }
}

impl FormatPrint for Declaration {
    fn format_print(&self, indent: usize) -> String {
        match self {
            Declaration::Function(decl) => decl.format_print(indent),
            Declaration::Record(decl) => decl.format_print(indent),
            Declaration::Resource(decl) => decl.format_print(indent),
            Declaration::Enum(decl) => decl.format_print(indent),
            Declaration::Variant(decl) => decl.format_print(indent),
            Declaration::Export(decl) => decl.format_print(indent),
            Declaration::Test(decl) => decl.format_print(indent),
        }
    }
}

impl FormatPrint for FunctionDeclaration {
    fn format_print(&self, indent: usize) -> String {
        format!(
            "{}func {}({}) {} {}",
            tab(indent),
            self.name,
            self.parameters.format_print(", ", 0),
            self.return_type
                .as_ref()
                .map_or_else(String::new, |t| t.format_print(0)),
            self.body.format_print(indent)
        )
    }
}

impl FormatPrint for Parameter {
    fn format_print(&self, indent: usize) -> String {
        format!(
            "{}{}: {}",
            tab(indent),
            self.name,
            self.type_annotation.format_print(0)
        )
    }
}

impl FormatPrint for RecordDeclaration {
    fn format_print(&self, indent: usize) -> String {
        let fields = self.fields.format_print("\n", indent + 1);
        let newline = if fields.is_empty() { "" } else { "\n" };

        format!(
            "{}record {} {{{newline}{fields}{newline}}}",
            tab(indent),
            self.name
        )
    }
}

impl FormatPrint for Field {
    fn format_print(&self, indent: usize) -> String {
        format!(
            "{}{}: {}",
            tab(indent),
            self.name,
            self.type_annotation.format_print(0)
        )
    }
}

impl FormatPrint for TypeAnnotation {
    fn format_print(&self, indent: usize) -> String {
        let ty = match self {
            TypeAnnotation::Simple(s) => s.clone(),
            TypeAnnotation::List(type_annotation) => {
                format!("[{}]", type_annotation.format_print(0))
            }
            TypeAnnotation::Option(type_annotation) => {
                format!("{}?", type_annotation.format_print(0))
            }
            TypeAnnotation::Result { ok, err } => {
                format!("{}!{}", ok.format_print(0), err.format_print(0))
            }
            TypeAnnotation::Tuple(_vec) => todo!(),
        };

        format!("{}{ty}", tab(indent))
    }
}

impl FormatPrint for ResourceDeclaration {
    fn format_print(&self, indent: usize) -> String {
        todo!()
    }
}

impl FormatPrint for EnumDeclaration {
    fn format_print(&self, indent: usize) -> String {
        let cases = self
            .cases
            .iter()
            .map(|c| format!("{}{c}", tab(indent + 1)))
            .collect::<Vec<_>>()
            .join("\n");
        let newline = if cases.is_empty() { "" } else { "\n" };
        format!(
            "{}enum {} {{{newline}{cases}{newline}}}",
            tab(indent),
            self.name
        )
    }
}

impl FormatPrint for VariantDeclaration {
    fn format_print(&self, indent: usize) -> String {
        let cases = self.cases.format_print("\n", indent + 1);
        let newline = if cases.is_empty() { "" } else { "\n" };
        format!(
            "{}variant {} {{{newline}{cases}{newline}}}",
            tab(indent),
            self.name
        )
    }
}

impl FormatPrint for VariantCase {
    fn format_print(&self, indent: usize) -> String {
        let associated = self.associated_types.format_print(", ", 0);
        format!(
            "{}{}{}{associated}{}",
            tab(indent),
            self.name,
            if associated.is_empty() { "" } else { "(" },
            if associated.is_empty() { "" } else { ")" },
        )
    }
}

impl FormatPrint for ExportDeclaration {
    fn format_print(&self, indent: usize) -> String {
        let inner = &self.item;

        format!("{}export {}", tab(indent), inner.format_print(0))
    }
}

impl FormatPrint for TestDeclaration {
    fn format_print(&self, indent: usize) -> String {
        let name = self
            .name
            .as_ref()
            .map_or_else(String::new, |n| format!("\"{n}\" "));
        let body = self.body.format_print(indent);

        format!("{}test {name}{body}", tab(indent))
    }
}

impl FormatPrint for Block {
    fn format_print(&self, indent: usize) -> String {
        let statements = self.statements.format_print("\n", indent + 1);
        let newline = if self.statements.is_empty() { "" } else { "\n" };
        format!("{{{newline}{statements}{newline}{}}}", tab(indent))
    }
}

impl FormatPrint for Statement {
    fn format_print(&self, indent: usize) -> String {
        let stmt = match self {
            Statement::VariableDeclaration(variable_declaration) => {
                format!(
                    "{} {}{}{}",
                    if variable_declaration.mutable {
                        "var"
                    } else {
                        "let"
                    },
                    variable_declaration.name,
                    variable_declaration
                        .type_annotation
                        .as_ref()
                        .map_or_else(String::new, |ty| format!(": {}", ty.format_print(0))),
                    variable_declaration
                        .value
                        .as_ref()
                        .map_or_else(String::new, |val| format!(" = {}", val.format_print(0)))
                )
            }
            Statement::Assignment { target, value } => {
                // TODO: check if right-hand side is binop with the first operand being the same as lhs and collapse to e.g. +=
                format!("{} = {}", target.format_print(0), value.format_print(0))
            }
            Statement::ExpressionStatement(expression) => expression.format_print(0),
            Statement::ReturnStatement(expression) => {
                format!(
                    // TODO: remove the \n if the return statement is the first thing in a new scope
                    "\n{}return{}",
                    tab(indent),
                    expression
                        .as_ref()
                        .map_or_else(String::new, |e| format!(" {}", e.format_print(0)))
                )
            }
            Statement::BreakStatement(expression) => {
                format!(
                    "break{}",
                    expression
                        .as_ref()
                        .map_or_else(String::new, |e| format!(" {}", e.format_print(0)))
                )
            }
            Statement::ContinueStatement => "continue".to_owned(),
            Statement::ThrowStatement(expression) => {
                format!("throw {}", expression.format_print(0))
            }
            Statement::IfStatement(if_statement) => if_statement.format_print(indent),
            Statement::MatchStatement(match_statement) => todo!(),
            Statement::WhileStatement(while_statement) => while_statement.format_print(indent),
            Statement::ForStatement(for_statement) => todo!(),
            Statement::Assertion(assert_statement) => {
                format!("assert {}", assert_statement.condition.format_print(0))
            }
        };

        format!("{}{stmt}", tab(indent))
    }
}

impl FormatPrint for IfStatement {
    fn format_print(&self, indent: usize) -> String {
        if let Some(ref else_branch) = self.else_branch {
            format!(
                "if {} {} else {}",
                self.condition.format_print(0),
                self.then_branch.format_print(indent),
                else_branch.format_print(indent),
            )
        } else {
            format!(
                "if {} {}",
                self.condition.format_print(0),
                self.then_branch.format_print(indent)
            )
        }
    }
}

impl FormatPrint for WhileStatement {
    fn format_print(&self, indent: usize) -> String {
        format!(
            "while {} {}",
            self.condition.format_print(0),
            self.body.format_print(indent)
        )
    }
}

impl FormatPrint for Expression {
    fn format_print(&self, indent: usize) -> String {
        let expr: Cow<str> = match self {
            Expression::Literal(literal) => literal.format_print(0).into(),
            Expression::Identifier(ident) => ident.into(),
            Expression::BinaryExpression {
                left,
                operator,
                right,
            } => format!(
                "{} {} {}",
                left.format_print(0),
                operator.format_print(0),
                right.format_print(0)
            )
            .into(),
            Expression::UnaryExpression { operator, operand } => {
                format!("{}{}", operator.format_print(0), operand.format_print(0)).into()
            }
            Expression::YeetExpression { expression } => todo!(),
            Expression::FunctionCall {
                function,
                arguments,
            } => format!(
                "{}({})",
                function.format_print(0),
                arguments.format_print(", ", 0)
            )
            .into(),
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
            Expression::ListLiteral(vec) => todo!(),
        };

        format!("{}{expr}", tab(indent))
    }
}

impl FormatPrint for BinaryOperator {
    fn format_print(&self, indent: usize) -> String {
        let op = match self {
            BinaryOperator::Arithmetic(arithmetic_operator) => match arithmetic_operator {
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

        format!("{}{}", tab(indent), op)
    }
}

impl FormatPrint for UnaryOperator {
    fn format_print(&self, indent: usize) -> String {
        let op = match self {
            UnaryOperator::Negate => "-",
            UnaryOperator::Not => "!",
        };

        format!("{}{}", tab(indent), op)
    }
}

impl FormatPrint for Literal {
    fn format_print(&self, indent: usize) -> String {
        let lit: Cow<str> = match self {
            Literal::Integer(i) => format!("{i}").into(),
            Literal::Float(f) => format!("{f}").into(),
            Literal::String(s) => s.into(),
            Literal::Boolean(b) => format!("{b}").into(),
            Literal::None => "none".into(),
        };

        format!("{}{}", tab(indent), lit)
    }
}

trait CollectionFormatPrint {
    fn format_print(&self, separator: &str, indent: usize) -> String;
}

impl<T> CollectionFormatPrint for [T]
where
    T: FormatPrint,
{
    fn format_print(&self, separator: &str, indent: usize) -> String {
        self.iter()
            .map(|e| e.format_print(indent))
            .collect::<Vec<_>>()
            .join(separator)
    }
}

impl<T> FormatPrint for Option<T>
where
    T: FormatPrint,
{
    fn format_print(&self, indent: usize) -> String {
        self.as_ref()
            .map_or_else(String::new, |e| e.format_print(indent))
    }
}

fn tab(n: usize) -> String {
    "\t".repeat(n)
}
