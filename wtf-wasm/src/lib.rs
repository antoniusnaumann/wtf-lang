use std::{
    cmp::max,
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

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Type {
    Simple(TypeRef),
    List(TypeRef),
    Option(TypeRef),
    Result { ok: TypeRef, err: TypeRef },
    Record { fields: Vec<(String, TypeRef)> },
    Variant {},
    Tuple(Vec<TypeRef>),
    Flags(Vec<String>),
    Enum(HashSet<String>),
    Own(),
    Borrow(),
}

impl From<TypeRef> for Type {
    fn from(value: TypeRef) -> Self {
        Type::Simple(value)
    }
}

impl From<&TypeRef> for Type {
    fn from(value: &TypeRef) -> Self {
        Type::Simple(*value)
    }
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
    component_types: Vec<(Type, Vec<ValType>)>,
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

        self.component_types = lower_types(instance.types);

        for func in instance.functions {
            if let Some(export) = self.encode_fn(func) {
                self.component_lookup.insert(export.0.to_owned(), export.2);
                exports.push(export);
            }
        }

        // self.component_instances.export_items(exports);
    }

    fn lower(&self, ty: &TypeRef) -> Vec<ValType> {
        match ty {
            TypeRef::Primitive(p) => p.lower(),
            TypeRef::Type(i) => self.component_types[*i as usize].1.clone(),
        }
    }

    fn encode_fn(&mut self, function: Function<'a>) -> Option<(String, ComponentExportKind, u32)> {
        let idx = self.encode_lowered_fn(
            function
                .params
                .iter()
                .flat_map(|(_, p)| self.lower(p))
                .collect(),
            function.result.map_or_else(|| vec![], |ty| self.lower(&ty)),
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

        for (_, (ty, _)) in self.component_types.into_iter().enumerate() {
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
                    encoder.record(fields.iter().map(|(s, t)| (s.as_str(), t.clone())))
                }
                Type::Variant {} => todo!(),
                Type::Tuple(fields) => encoder.tuple(fields),
                Type::Flags(names) => encoder.flags(names.iter().map(String::as_str)),
                Type::Enum(cases) => encoder.enum_type(cases.iter().map(String::as_str)),
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

fn lower_types(types: Vec<Type>) -> Vec<(Type, Vec<ValType>)> {
    let mut result: Vec<Option<Vec<ValType>>> = types.iter().map(|_| None).collect();

    for (index, ty) in types.iter().enumerate() {
        if result[index].is_some() {
            continue;
        }

        result[index] = Some(lower_type(ty, &types, &mut result));
    }

    let mapping = result.into_iter().map(|lowered| lowered.unwrap());

    types.into_iter().zip(mapping).collect()
}

fn lower_type(ty: &Type, types: &[Type], mapping: &mut [Option<Vec<ValType>>]) -> Vec<ValType> {
    match ty {
        Type::Simple(t) => match t {
            TypeRef::Primitive(p) => p.lower(),
            TypeRef::Type(i) => {
                let i = *i as usize;
                let mapped = &mapping[i];
                match &mapped {
                    Some(types) => types.clone(),
                    None => {
                        let types = lower_type(&types[i], &types, mapping);
                        mapping[i] = Some(types.clone());

                        types
                    }
                }
            }
        },
        Type::List(_) => vec![ValType::I32, ValType::I32],
        Type::Option(inner) => {
            let inner = lower_type(&inner.into(), types, mapping);

            // Discriment (i32) + lowered payload
            [vec![ValType::I32], inner].concat()
        }
        Type::Result { ok, err } => {
            let ok = lower_type(&ok.into(), types, mapping);
            let err = lower_type(&err.into(), types, mapping);

            let mut payload = Vec::new();

            for index in 0..max(ok.len(), err.len() - 1) {
                // According to: https://github.com/WebAssembly/component-model/blob/main/design/mvp/CanonicalABI.md#flattening
                let overlap = match (ok.get(index), err.get(index)) {
                    (Some(a), Some(b)) => {
                        if a == b {
                            *a
                        } else if matches!(
                            (a, b),
                            (ValType::I32 | ValType::F32, ValType::I32 | ValType::F32)
                        ) {
                            ValType::I32
                        } else {
                            ValType::I64
                        }
                    }
                    (None, Some(v)) | (Some(v), None) => *v,
                    (None, None) => unreachable!(),
                };
                payload.push(overlap)
            }

            // Discriminent (i32) + overlap between lowered ok and lowered err
            [vec![ValType::I32], payload].concat()
        }
        Type::Record { fields } => fields
            .into_iter()
            .flat_map(|(_, ty)| lower_type(&ty.into(), types, mapping))
            .collect(),
        Type::Variant {} => todo!(),
        Type::Tuple(fields) => fields
            .into_iter()
            .flat_map(|ty| lower_type(&ty.into(), types, mapping))
            .collect(),
        Type::Flags(flags) => vec![if flags.len() > 32 {
            ValType::I64
        } else {
            ValType::I32
        }],
        Type::Enum(_) => vec![ValType::I32],
        Type::Own() => vec![ValType::I32],
        Type::Borrow() => vec![ValType::I32],
    }
}

trait CanonicalLowering {
    type Out;

    fn lower(&self) -> Self::Out;
}

impl CanonicalLowering for PrimitiveValType {
    type Out = Vec<ValType>;

    fn lower(&self) -> Self::Out {
        match self {
            PrimitiveValType::Bool
            | PrimitiveValType::S8
            | PrimitiveValType::U8
            | PrimitiveValType::S16
            | PrimitiveValType::U16
            | PrimitiveValType::S32
            | PrimitiveValType::U32 => vec![ValType::I32],
            PrimitiveValType::S64 | PrimitiveValType::U64 | PrimitiveValType::Char => {
                vec![ValType::I64]
            }
            PrimitiveValType::F32 => vec![ValType::F32],
            PrimitiveValType::F64 => vec![ValType::F64],
            PrimitiveValType::String => vec![ValType::I32, ValType::I32],
        }
    }
}
