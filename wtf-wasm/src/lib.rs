use std::{
    collections::{HashMap, HashSet},
    ops::Deref,
};

use wasm_encoder::{
    CodeSection, ComponentExportKind, ConstExpr, DataSection, ExportKind, ExportSection,
    FunctionSection, MemorySection, MemoryType, Module, PrimitiveValType, TypeSection, ValType,
};

// wasm encoder re-exports
pub use wasm_encoder::{
    ComponentValType as TypeRef, Instruction, PrimitiveValType as PrimitiveType,
};

#[derive(Debug, PartialEq, Eq)]
pub enum Type {
    Simple(TypeRef),
    List(TypeRef),
    Option(TypeRef),
    Result { ok: TypeRef, err: TypeRef },
    Record { fields: Vec<(String, TypeRef)> },
    Variant {},
    Tuple(),
    Flags(),
    Enum(HashSet<String>),
    Own(),
    Borrow(),
}

#[derive(Debug)]
pub struct Function<'a> {
    pub params: Vec<(String, TypeRef)>,
    pub result: Option<TypeRef>,
    pub name: String,
    pub instructions: Vec<Instruction<'a>>,
    pub export: bool,
}

#[derive(Debug)]
pub struct Instance<'a> {
    pub name: String,
    pub functions: Vec<Function<'a>>,
    pub types: Vec<Type>,
}

#[derive(Debug, Default)]
pub struct ComponentBuilder<'a> {
    types: TypeSection,
    signatures: FunctionSection,
    exports: ExportSection,
    codes: CodeSection,

    functions: Vec<(u32, Function<'a>)>,
    module_count: u32,
    component_count: u32,
    component_lookup: HashMap<String, u32>,
    component_types: Vec<Type>,
    inner: wasm_encoder::ComponentBuilder,

    has_instance: bool,
}

// Probably embed https://docs.rs/wasm-encoder/latest/wasm_encoder/struct.ComponentBuilder.html and use the lower_func method from there
impl<'a> ComponentBuilder<'a> {
    pub fn new() -> ComponentBuilder<'a> {
        Self::default()
    }

    pub fn encode_instance(&mut self, instance: Instance<'a>) {
        if self.has_instance {
            panic!("Currently only one instance per component is supported.")
        }
        self.has_instance = true;

        let mut exports = Vec::new();

        self.component_types = instance.types;

        for func in instance.functions {
            if let Some(export) = self.encode_fn(func) {
                self.component_lookup.insert(export.0.to_owned(), export.2);
                exports.push(export);
            }
        }

        // self.component_instances.export_items(exports);
    }

    fn encode_fn(&mut self, function: Function<'a>) -> Option<(String, ComponentExportKind, u32)> {
        use CanonicalLowering as Lower;
        let idx = self.encode_lowered_fn(
            function.params.iter().map(Lower::lower).collect(),
            function.result.lower(),
            &function.name,
            &function.instructions,
        );

        let result = if function.export {
            Some((
                function.name.clone(),
                ComponentExportKind::Func,
                self.component_count,
            ))
        } else {
            None
        };

        self.functions.push((idx, function));
        self.component_count += 1;

        result
    }

    fn encode_lowered_fn(
        &mut self,
        params: Vec<ValType>,
        results: Vec<ValType>,
        name: impl AsRef<str>,
        instructions: &[Instruction],
    ) -> u32 {
        self.types.function(params, results);

        // Encode the function section.
        self.signatures.function(self.module_count);

        // Encode the export section.
        self.exports
            .export(name.as_ref(), ExportKind::Func, self.module_count);

        let locals = vec![];
        let mut f = wasm_encoder::Function::new(locals);
        for stmt in instructions {
            f.instruction(stmt);
        }
        self.codes.function(&f);

        let result = self.module_count;
        self.module_count += 1;

        result
    }

    pub fn finish(mut self) -> Vec<u8> {
        let (memory, data) = self.mem();
        let mut module = Module::new();
        module.section(&self.types);
        module.section(&self.signatures);
        module.section(&memory);
        module.section(&self.exports);
        module.section(&self.codes);
        module.section(&data);
        self.inner.core_module(&module);
        self.inner.core_instantiate(0, vec![]);

        let mut exports = vec![];

        for (_, ty) in self.component_types.into_iter().enumerate() {
            let (_, encoder) = self.inner.type_defined();
            match ty {
                Type::Simple(ty) => match ty {
                    TypeRef::Primitive(ty) => encoder.primitive(ty),
                    TypeRef::Type(ty) => {
                        panic!("Expected an concrete type here, found {:#?}!", ty)
                    }
                },
                Type::List(element) => encoder.list(element),
                Type::Option(inner) => encoder.option(inner),
                Type::Result { ok, err } => encoder.result(Some(ok), Some(err)),
                Type::Record { fields } => {
                    todo!()
                }
                Type::Variant {} => todo!(),
                Type::Tuple() => todo!(),
                Type::Flags() => todo!(),
                Type::Enum(cases) => encoder.enum_type(cases.iter().map(|s| s.as_str())),
                Type::Own() => todo!(),
                Type::Borrow() => todo!(),
            }
        }

        for (idx, function) in self.functions {
            let (type_idx, mut encoder) = self.inner.type_function();
            encoder.params(
                function
                    .params
                    .iter()
                    .map(|(s, t)| (s.deref(), t.clone()))
                    .collect::<Vec<_>>(),
            );
            if let Some(result) = function.result {
                encoder.result(result);
            } else {
                let r: [(&str, TypeRef); 0] = [];
                encoder.results(r);
            }
            self.inner
                .core_alias_export(0, &function.name, ExportKind::Func);
            self.inner.lift_func(idx, type_idx, vec![]);
            if function.export {
                exports.push((function.name, idx))
            }
        }

        for (name, idx) in exports {
            self.inner
                .export(&name, ComponentExportKind::Func, idx, None);
        }

        self.inner.finish()
    }

    fn mem(&self) -> (MemorySection, DataSection) {
        let mut memory = MemorySection::new();
        memory.memory(MemoryType {
            minimum: 1,
            maximum: None,
            memory64: false,
            shared: false,
            page_size_log2: None,
        });

        let mut data = DataSection::new();
        let memory_index = 0;
        let offset = ConstExpr::i32_const(42);
        let segment_data = b"hello";
        data.active(memory_index, &offset, segment_data.iter().copied());

        (memory, data)
    }
}

trait CanonicalLowering {
    type Out;

    fn lower(&self) -> Self::Out;
}

impl CanonicalLowering for TypeRef {
    type Out = ValType;

    fn lower(&self) -> Self::Out {
        match self {
            TypeRef::Primitive(p) => match p {
                PrimitiveValType::Bool
                | PrimitiveValType::S8
                | PrimitiveValType::U8
                | PrimitiveValType::S16
                | PrimitiveValType::U16
                | PrimitiveValType::S32
                | PrimitiveValType::U32 => ValType::I32,
                PrimitiveValType::S64 | PrimitiveValType::U64 | PrimitiveValType::Char => {
                    ValType::I64
                }
                PrimitiveValType::F32 => ValType::F32,
                PrimitiveValType::F64 => ValType::F64,
                PrimitiveValType::String => ValType::I32,
            },
            TypeRef::Type(t) => todo!("Handle type references"),
        }
    }
}

impl CanonicalLowering for (String, TypeRef) {
    type Out = ValType;

    fn lower(&self) -> Self::Out {
        self.1.lower()
    }
}

impl CanonicalLowering for Option<TypeRef> {
    type Out = Vec<ValType>;

    fn lower(&self) -> Self::Out {
        if let Some(inner) = self {
            vec![inner.lower()]
        } else {
            Vec::new()
        }
    }
}
