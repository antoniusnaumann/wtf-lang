use wtf_ast::{Declaration, Module, ModulePath, PackageDeclaration, UseDeclaration, Version};

trait FormatPrint {
    fn format_print(&self) -> String;
}

impl FormatPrint for Module {
    fn format_print(&self) -> String {
        let package = self.package.format_print();
        let uses = self.uses.format_print();
        let declarations = self.declarations.format_print();

        format!(
            "{package}{}{uses}{}{declarations}",
            if !package.is_empty() { "\n\n" } else { "" },
            if !uses.is_empty() { "\n\n" } else { "" }
        )
    }
}

impl FormatPrint for PackageDeclaration {
    fn format_print(&self) -> String {
        let path = self.path.format_print();
        let version = self.version.format_print();

        format!()
    }
}

impl FormatPrint for ModulePath {
    fn format_print(&self) -> String {
        todo!()
    }
}

impl FormatPrint for Version {
    fn format_print(&self) -> String {
        todo!()
    }
}

impl FormatPrint for UseDeclaration {
    fn format_print(&self) -> String {
        todo!()
    }
}

impl FormatPrint for Declaration {
    fn format_print(&self) -> String {
        todo!()
    }
}

impl<T> FormatPrint for [T]
where
    T: FormatPrint,
{
    fn format_print(&self) -> String {
        self.iter().map(T::format_print).collect()
    }
}

impl<T> FormatPrint for Option<T>
where
    T: FormatPrint,
{
    fn format_print(&self) -> String {
        self.as_ref()
            .map_or_else(String::new, FormatPrint::format_print)
    }
}
