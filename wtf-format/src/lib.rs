use wtf_ast::{
    Block, Declaration, EnumDeclaration, ExportDeclaration, Field, FunctionDeclaration, Module,
    ModulePath, PackageDeclaration, RecordDeclaration, ResourceDeclaration, TestDeclaration,
    TypeAnnotation, UseDeclaration, VariantDeclaration, Version,
};

trait FormatPrint {
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
            "{}{path}{}",
            "\t".repeat(indent),
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
        todo!()
    }
}

impl FormatPrint for RecordDeclaration {
    fn format_print(&self, indent: usize) -> String {
        let fields = self.fields.format_print("\n", indent + 1);
        let newline = if fields.is_empty() { "" } else { "\n" };

        format!("record {} {{{newline}{fields}{newline}}}", self.name)
    }
}

impl FormatPrint for Field {
    fn format_print(&self, indent: usize) -> String {
        format!(
            "{}{}: {}",
            "\t".repeat(indent),
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

        format!("{}{ty}", "\t".repeat(indent))
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
            .map(|c| format!("{}{c}", "\t".repeat(indent + 1)))
            .collect::<Vec<_>>()
            .join("\n");
        let newline = if cases.is_empty() { "" } else { "\n" };
        format!("enum {} {{{newline}{cases}{newline}}}", self.name)
    }
}

impl FormatPrint for VariantDeclaration {
    fn format_print(&self, indent: usize) -> String {
        todo!()
    }
}

impl FormatPrint for ExportDeclaration {
    fn format_print(&self, indent: usize) -> String {
        let inner = &self.item;

        format!("{}export {}", "\t".repeat(indent), inner.format_print(0))
    }
}

impl FormatPrint for TestDeclaration {
    fn format_print(&self, indent: usize) -> String {
        let name = self
            .name
            .as_ref()
            .map_or_else(String::new, |n| format!("\"{n}\" "));
        let body = self.body.format_print(indent + 1);

        format!("{}test {name}{body}", "\t".repeat(indent))
    }
}

impl FormatPrint for Block {
    fn format_print(&self, indent: usize) -> String {
        todo!()
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
