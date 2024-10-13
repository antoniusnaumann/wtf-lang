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

impl Print for Vec<String> {
    fn print(&self, f: &mut std::fmt::Formatter<'_>, indent: usize, c: char) -> std::fmt::Result {
        let strings = self.join(" ");
        write!(f, "\n{c:indent$}({})", strings)
    }
}

impl Print for Vec<UseDeclaration> {
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

impl Print for Vec<Declaration> {
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
        todo!()
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

impl Print for Vec<Parameter> {
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
        write!(f, "\n{c:indent$}TODO: Block")
    }
}
