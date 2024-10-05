use std::{
    collections::{HashMap, HashSet},
    fmt::Debug,
    ops::Deref,
};

use wasm_encoder::{
    CanonicalOption, CodeSection, ComponentExportKind, ConstExpr, DataSection, ExportKind,
    ExportSection, FunctionSection, GlobalSection, GlobalType, MemorySection, MemoryType, Module,
    PrimitiveValType, TypeSection, ValType,
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
    globals: GlobalSection,

    functions: Vec<(u32, Function<'a>)>,
    function_count: u32,
    global_count: u32,
    component_count: u32,
    component_lookup: HashMap<String, u32>,
    component_types: Vec<Type>,
    inner: wasm_encoder::ComponentBuilder,

    realloc_index: u32,
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
        self.signatures.function(self.function_count);

        // Encode the export section.
        self.exports
            .export(name.as_ref(), ExportKind::Func, self.function_count);

        let locals = vec![];
        let mut f = wasm_encoder::Function::new(locals);
        for stmt in instructions {
            f.instruction(stmt);
        }
        self.codes.function(&f);

        let result = self.function_count;
        self.function_count += 1;

        result
    }

    pub fn finish(mut self) -> Vec<u8> {
        self.realloc();
        let (memory, data) = self.mem();
        self.exports.export("memory", ExportKind::Memory, 0);
        let mut module = Module::new();
        module.section(&self.types);
        module.section(&self.signatures);
        module.section(&memory);
        module.section(&self.globals);
        module.section(&self.exports);
        module.section(&self.codes);
        module.section(&data);
        self.inner.core_module(&module);
        self.inner.core_instantiate(0, vec![]);
        self.inner
            .alias_core_export(0, "memory", ExportKind::Memory);

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

        for (_, function) in &self.functions {
            self.inner
                .core_alias_export(0, &function.name, ExportKind::Func);
        }

        self.inner.core_alias_export(0, "realloc", ExportKind::Func);

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
            self.inner.lift_func(
                idx,
                type_idx,
                vec![
                    CanonicalOption::Memory(0),
                    CanonicalOption::Realloc(self.realloc_index),
                ],
            );
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

    // Creates the realloc function
    fn realloc(&mut self) {
        self.types.function(
            vec![ValType::I32, ValType::I32, ValType::I32, ValType::I32],
            vec![ValType::I32],
        );

        self.signatures.function(self.function_count);
        self.exports
            .export("realloc", ExportKind::Func, self.function_count);

        // Global $heap_ptr (mut i32) = initial value 65536
        self.globals.global(
            GlobalType {
                val_type: ValType::I32,
                mutable: true,
                shared: false,
            },
            &ConstExpr::i32_const(65536),
        );
        let heap_ptr = self.global_count;
        self.global_count += 1;

        // Declare local $ret
        let mut realloc_func = wasm_encoder::Function::new(vec![(1, ValType::I32)]);

        // Function body:
        // param $ptr i32       ; local index 0
        // param $old_size i32  ; local index 1
        // param $align i32     ; local index 2
        // param $new_size i32  ; local index 3
        // local $ret i32       ; local index 4

        // TODO: This is currently a simple bump allocator that never reallocates. Use some library realloc function instead

        // Get the current heap pointer and store it in $ret
        realloc_func.instruction(&Instruction::GlobalGet(heap_ptr)); // global.get $heap_ptr
        realloc_func.instruction(&Instruction::LocalSet(4)); // local.set $ret

        // Update the heap pointer: $heap_ptr += $new_size
        realloc_func.instruction(&Instruction::GlobalGet(heap_ptr)); // global.get $heap_ptr
        realloc_func.instruction(&Instruction::LocalGet(3)); // local.get $new_size
        realloc_func.instruction(&Instruction::I32Add); // i32.add
        realloc_func.instruction(&Instruction::GlobalSet(heap_ptr)); // global.set $heap_ptr

        // Return the original heap pointer (stored in $ret)
        realloc_func.instruction(&Instruction::LocalGet(4)); // local.get $ret
        realloc_func.instruction(&Instruction::End);

        self.codes.function(&realloc_func);
        self.realloc_index = self.function_count;
        self.function_count += 1;
    }
}

trait CanonicalLowering {
    type Out;

    fn lower(&self) -> Self::Out;
}

impl CanonicalLowering for TypeRef {
    // TODO: It might be neccessary to return multiple values here
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
            TypeRef::Type(t) => ValType::I32, // todo!("Handle ref types"),
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
