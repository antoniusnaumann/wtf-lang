use std::{
    cmp::max,
    collections::{HashMap, HashSet},
    fmt::Debug,
    iter,
    ops::Deref,
};

use wasm_encoder::{
    BlockType, CanonicalOption, CodeSection, ComponentExportKind, ConstExpr, DataSection,
    ExportKind, ExportSection, FunctionSection, GlobalSection, GlobalType, MemArg, MemorySection,
    MemoryType, Module, PrimitiveValType, TypeSection, ValType,
};

mod instruction;
pub use instruction::*;

// wasm encoder re-exports
pub use wasm_encoder::{
    ComponentValType as TypeRef, Instruction as WasmInstruction, PrimitiveValType as PrimitiveType,
};

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct TypeDeclaration {
    pub name: String,
    pub ty: Type,
    pub export: bool,
}

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

#[derive(Debug, Clone)]
pub struct Function<'a> {
    pub signature: Signature,
    pub locals: Vec<TypeRef>,
    pub instructions: Vec<Instruction<'a>>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Signature {
    pub params: Vec<(String, TypeRef)>,
    pub result: Option<TypeRef>,
    pub name: String,
    pub export: bool,
}

#[derive(Debug, Clone)]
pub struct Local {
    /// Offset of the lowered locals in of this type
    pub index: u32,
    pub ty: TypeRef,
    pub lower: Vec<ValType>,
}

#[derive(Debug)]
pub struct Instance<'a> {
    pub name: String,
    pub functions: Vec<Function<'a>>,
    pub types: Vec<TypeDeclaration>,
    pub constants: HashSet<Vec<u8>>,
}

#[derive(Debug)]
struct ConstantPosition {
    offset: u32,
    length: u32,
}

#[derive(Debug, Default)]
pub struct ComponentBuilder {
    types: TypeSection,
    signatures: FunctionSection,
    exports: ExportSection,
    codes: CodeSection,
    globals: GlobalSection,

    // lowered id is the index
    functions: Vec<Signature>,
    function_count: u32,
    global_count: u32,
    component_count: u32,
    component_lookup: HashMap<String, u32>,
    component_types: Vec<(Type, Vec<ValType>)>,
    type_declarations: Vec<TypeDeclaration>,
    inner: wasm_encoder::ComponentBuilder,
    constants: HashMap<Vec<u8>, ConstantPosition>,
    data_ptr: u32,
    constant_ptr: u32,

    realloc_index: u32,
    has_instance: bool,
}

// Probably embed https://docs.rs/wasm-encoder/latest/wasm_encoder/struct.ComponentBuilder.html and use the lower_func method from there
impl ComponentBuilder {
    pub fn new() -> ComponentBuilder {
        Self::default()
    }

    pub fn encode_instance(&mut self, instance: Instance<'_>) {
        if self.has_instance {
            panic!("Currently only one instance per component is supported.")
        }
        self.has_instance = true;
        let mut offset = 0;

        for constant in instance.constants {
            let length = constant.len() as u32;
            self.constants
                .insert(constant, ConstantPosition { offset, length });
            offset += length;
        }
        // 64bit alignment
        self.constant_ptr = offset;
        self.data_ptr = offset + offset % 64;

        let mut exports = Vec::new();

        self.component_types = lower_types(&instance.types);
        self.type_declarations = instance.types;
        self.functions = instance
            .functions
            .iter()
            .map(|f| f.signature.clone())
            .collect();

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

    fn encode_fn(&mut self, function: Function<'_>) -> Option<(String, ComponentExportKind, u32)> {
        let idx = self.encode_lowered_fn(&function);

        let result = if function.signature.export {
            Some((
                function.signature.name.clone(),
                ComponentExportKind::Func,
                self.component_count,
            ))
        } else {
            None
        };

        assert!(function.signature == self.functions[idx as usize]);
        self.component_count += 1;

        result
    }

    fn encode_lowered_fn(&mut self, function: &Function<'_>) -> u32 {
        let results = function
            .signature
            .result
            .map_or_else(|| vec![], |ty| self.lower(&ty));

        let mut locals: Vec<Local> = self.enumerate_locals(&function.locals);
        let mut intermediate_locals = vec![];
        // Additional instructions and locals to handle multi-value return functions
        let mut additional_instructions = vec![];
        let results = if results.len() <= 1 {
            results
        } else {
            // The component proposal currently does not support multi-value return types, they therefore have to be transferred using the linear memory: https://github.com/WebAssembly/component-model/blob/main/design/mvp/CanonicalABI.md#flattening
            let mut local_idx = locals.iter().fold(0, |acc, local| acc + local.lower.len());
            let mem_arg = MemArg {
                offset: 0,
                align: 0,
                memory_index: 0,
            };
            let offset = self.data_ptr;
            self.data_ptr = results.iter().fold(self.data_ptr, |acc, result| {
                additional_instructions.push(WasmInstruction::I32Const(acc as i32));
                additional_instructions.push(WasmInstruction::LocalGet(local_idx as u32));
                let len = match result {
                    ValType::I32 => {
                        additional_instructions.push(WasmInstruction::I32Store(mem_arg));
                        intermediate_locals.push(Local {
                            index: local_idx as u32,
                            ty: TypeRef::Primitive(PrimitiveValType::S32),
                            lower: vec![ValType::I32],
                        });
                        4
                    }
                    ValType::I64 => {
                        additional_instructions.push(WasmInstruction::I64Store(mem_arg));
                        intermediate_locals.push(Local {
                            index: local_idx as u32,
                            ty: TypeRef::Primitive(PrimitiveValType::S64),
                            lower: vec![ValType::I64],
                        });
                        8
                    }
                    ValType::F32 => {
                        additional_instructions.push(WasmInstruction::F32Store(mem_arg));
                        intermediate_locals.push(Local {
                            index: local_idx as u32,
                            ty: TypeRef::Primitive(PrimitiveValType::F32),
                            lower: vec![ValType::F32],
                        });
                        4
                    }
                    ValType::F64 => {
                        additional_instructions.push(WasmInstruction::F64Store(mem_arg));
                        intermediate_locals.push(Local {
                            index: local_idx as u32,
                            ty: TypeRef::Primitive(PrimitiveValType::F64),
                            lower: vec![ValType::F64],
                        });
                        8
                    }
                    ValType::V128 => todo!("V128 in multiple return type"),
                    ValType::Ref(_) => todo!("Ref-Type in multiple return type"),
                };
                local_idx += 1;
                acc + len
            });
            additional_instructions = intermediate_locals
                .iter()
                // it seems that this .rev() call is wrong here, but I don't know why
                // .rev()
                .map(|local| WasmInstruction::LocalSet(local.index))
                .chain(additional_instructions)
                .chain(iter::once(WasmInstruction::I32Const(offset as i32)))
                .collect();

            vec![ValType::I32]
        };
        locals.extend(intermediate_locals);

        let params: Vec<Vec<ValType>> = function
            .signature
            .params
            .iter()
            .map(|(_, p)| self.lower(p))
            .collect();
        let mut offsets = Vec::with_capacity(params.len() + 1);
        offsets.push(0);
        for param in &params {
            offsets.push(offsets.last().unwrap() + param.len());
        }

        self.types.ty().function(
            params.iter().flatten().cloned().collect::<Vec<_>>(),
            results,
        );

        // Encode the function section.
        self.signatures.function(self.function_count);

        // Encode the export section.
        self.exports.export(
            &function.signature.name.as_ref(),
            ExportKind::Func,
            self.function_count,
        );

        let mut func = wasm_encoder::Function::new_with_locals_types(
            lower_locals(locals.iter().skip(function.signature.params.len()))
                .map(|(_, ty)| ty)
                .collect::<Vec<_>>(),
        );

        for instruction in &function.instructions {
            let mut nesting_deph = Vec::new();
            for lowered_instruction in
                self.lower_instruction(instruction, &locals, &mut nesting_deph)
            {
                if additional_instructions.len() > 0
                    && matches!(lowered_instruction, WasmInstruction::Return)
                {
                    for additional in &additional_instructions {
                        func.instruction(additional);
                    }
                }
                func.instruction(&lowered_instruction);
            }
        }
        self.codes.function(&func);

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
            .core_alias_export(0, "memory", ExportKind::Memory);

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

        for function in &self.functions {
            self.inner
                .core_alias_export(0, &function.name, ExportKind::Func);
        }

        self.inner.core_alias_export(0, "realloc", ExportKind::Func);

        for (idx, function) in self.functions.iter().enumerate() {
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
                idx as u32,
                type_idx,
                vec![
                    CanonicalOption::Memory(0),
                    CanonicalOption::Realloc(self.realloc_index),
                ],
            );
            if function.export {
                exports.push((&function.name, idx, ComponentExportKind::Func));
            }
        }

        // TODO: Make sure that types match here, e.g. by doing a lookup for the index instead of enumerating
        for (idx, ty) in self.type_declarations.iter().enumerate() {
            continue;
            // TODO: This does not work currently
            if ty.export {
                exports.push((&ty.name, idx, ComponentExportKind::Type));
            }
        }

        for (name, idx, kind) in exports {
            self.inner.export(&name, kind, idx as u32, None);
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
        let offset = ConstExpr::i32_const(0);
        let mut segment_data = vec![0; self.constant_ptr as usize];
        for (constant, ConstantPosition { offset, length }) in &self.constants {
            let offset = *offset as usize;
            let length = *length as usize;
            for (index, byte) in constant.iter().enumerate() {
                debug_assert!(offset + length > offset + index);
                debug_assert!(segment_data[offset + index] == 0);
                segment_data[offset + index] = *byte;
            }
        }
        data.active(memory_index, &offset, segment_data);

        (memory, data)
    }

    // Creates the realloc function
    fn realloc(&mut self) {
        self.types.ty().function(
            vec![ValType::I32, ValType::I32, ValType::I32, ValType::I32],
            vec![ValType::I32],
        );

        self.signatures.function(self.function_count);
        self.exports
            .export("realloc", ExportKind::Func, self.function_count);

        // Global $heap_ptr (mut i32) = initial value 65536, the first page is used for constants
        self.globals.global(
            GlobalType {
                val_type: ValType::I32,
                mutable: true,
                shared: false,
            },
            &ConstExpr::i32_const(0x10000),
        );
        let heap_ptr = self.global_count;
        self.global_count += 1;

        // Page pointer, that always points to the end of the current page
        self.globals.global(
            GlobalType {
                val_type: ValType::I32,
                mutable: true,
                shared: false,
            },
            &ConstExpr::i32_const(0xFFFF),
        );
        let page_pointer = self.global_count;
        self.global_count += 1;

        // Declare local $ret
        let mut realloc_func = wasm_encoder::Function::new(vec![(2, ValType::I32)]);

        // TODO: free memory and use these
        let _param_ptr = 0;
        let _param_old_size = 1;
        // TODO: should we use alignment here?
        let _param_align = 2;
        let param_new_size = 3;
        let local_ret = 4;
        let local_new_heap = 5;

        // TODO: This is currently a simple bump allocator that never reallocates. Use some library realloc function instead

        let instructions = {
            use WasmInstruction::*;
            [
                // Get the current heap pointer and store it in $ret
                GlobalGet(heap_ptr),
                LocalSet(local_ret),
                // Update the heap pointer: $heap_ptr += $new_size
                GlobalGet(heap_ptr),
                LocalGet(param_new_size),
                I32Add,
                LocalTee(local_new_heap),
                GlobalSet(heap_ptr),
                // Grow memory if page limit was surpassed
                GlobalGet(page_pointer),
                LocalGet(local_new_heap),
                I32LeU,
                // TODO: calculate how many pages we should grow here
                If(BlockType::Empty),
                GlobalGet(page_pointer),
                I32Const(0x10000),
                I32Add,
                GlobalSet(page_pointer),
                // Grow memory by one page
                I32Const(1),
                MemoryGrow(0),
                I32Const(-1),
                I32Eq,
                If(BlockType::Empty),
                Unreachable,
                End,
                End,
                // Return the original heap pointer (stored in $ret)
                LocalGet(local_ret),
                End,
            ]
        };

        for instruction in &instructions {
            realloc_func.instruction(instruction);
        }

        self.codes.function(&realloc_func);
        self.realloc_index = self.function_count;
        self.function_count += 1;
    }

    fn lower_instruction<'a>(
        &'a self,
        instruction: &'a Instruction,
        locals: &[Local],
        nesting_depth: &mut Vec<BranchLabel>,
    ) -> Vec<WasmInstruction<'a>> {
        match instruction {
            // Locals
            Instruction::LocalSet(idx) => lower_local(&locals[*idx as usize])
                .iter()
                .map(|(i, _ty)| WasmInstruction::LocalSet(*i))
                .collect(),
            Instruction::LocalGet(idx) => lower_local(&locals[*idx as usize])
                .iter()
                .map(|(i, _ty)| WasmInstruction::LocalGet(*i))
                .collect(),

            Instruction::LocalGetMember { id, member } => {
                if member.len() > 1 {
                    todo!("Implement access chains");
                }
                // TODO: This only works if the struct is only composed of primitives
                let id = *id as usize;
                let ty = match locals[id].ty {
                    TypeRef::Primitive(_) => {
                        panic!("ERROR: field access on primitive not allowed")
                    }
                    TypeRef::Type(ty) => &self.type_declarations[ty as usize],
                };
                let Type::Record { fields } = &ty.ty else {
                    panic!("ERROR: field access on non-records not allowed")
                };
                let lower = lower_local(&locals[id]);

                vec![WasmInstruction::LocalGet(lower[member[0] as usize].0)]
            }

            Instruction::Bytes(bytes) => {
                let ConstantPosition { offset, length } = self.constants[bytes];
                vec![
                    WasmInstruction::I32Const(offset as i32),
                    WasmInstruction::I32Const(length as i32),
                ]
            }
            Instruction::I32(num) => vec![WasmInstruction::I32Const(*num)],
            Instruction::I64(num) => vec![WasmInstruction::I64Const(*num)],
            Instruction::F32(num) => vec![WasmInstruction::F32Const(*num)],
            Instruction::F64(num) => vec![WasmInstruction::F64Const(*num)],
            Instruction::Call(ident) => vec![self.lower_call(ident)],

            // Control Flow
            Instruction::End => vec![WasmInstruction::End],
            Instruction::If { then, else_ } => {
                nesting_depth.push(BranchLabel::If);
                let lowered = iter::once(WasmInstruction::If(BlockType::Empty))
                    .chain(
                        then.iter()
                            .flat_map(|inst| self.lower_instruction(inst, locals, nesting_depth))
                            .collect::<Vec<_>>(),
                    )
                    .chain(iter::once(WasmInstruction::Else))
                    .chain(
                        else_
                            .iter()
                            .flat_map(|inst| self.lower_instruction(inst, locals, nesting_depth)),
                    )
                    .chain(iter::once(WasmInstruction::End))
                    .collect();
                nesting_depth.pop();

                lowered
            }
            Instruction::Loop(block) => {
                nesting_depth.push(BranchLabel::Loop);
                nesting_depth.push(BranchLabel::LoopBlock);
                let lowered = [
                    WasmInstruction::Loop(BlockType::Empty),
                    WasmInstruction::Block(BlockType::Empty),
                ]
                .into_iter()
                .chain(
                    block
                        .into_iter()
                        .flat_map(|inst| self.lower_instruction(inst, locals, nesting_depth)),
                )
                .chain(
                    [
                        // This index counts inside out, so it should always branch to the loop
                        WasmInstruction::Br(1),
                        WasmInstruction::End,
                        WasmInstruction::End,
                    ]
                    .into_iter(),
                )
                .collect();
                nesting_depth.pop();
                nesting_depth.pop();

                lowered
            }
            Instruction::Block => vec![WasmInstruction::Block(todo!())],
            // This implicitly builds onto the knowledge that we always encode loops as loop + block and (to break out of the loop) we need to break out of the block that unconditionally branches to the loop otherwise
            // TODO: count loop depth to handle nested loops
            Instruction::Break => {
                let Some(index) = nesting_depth
                    .iter()
                    .rev()
                    .enumerate()
                    .find_map(|(i, label)| match label {
                        BranchLabel::LoopBlock => Some(i),
                        _ => None,
                    })
                else {
                    panic!("Break called outside of loop")
                };
                vec![WasmInstruction::Br(index as u32)]
            }
            // TODO: count loop depth to handle nested loops
            Instruction::Continue => {
                let Some(index) = nesting_depth
                    .iter()
                    .rev()
                    .enumerate()
                    .find_map(|(i, label)| match label {
                        BranchLabel::Loop => Some(i),
                        _ => None,
                    })
                else {
                    panic!("Continue called outside of loop")
                };
                vec![WasmInstruction::Br(index as u32)]
            }
            Instruction::Return => {
                // TODO: Put return values in register if string is returned
                vec![WasmInstruction::Return]
            }
            Instruction::Pop => vec![], // vec![WasmInstruction::Drop], // TODO: Figure out how and where to handle dropping of more complex types (that lower to more than one value)
            Instruction::Noop => vec![],

            Instruction::Wasm(wasm) => vec![wasm.clone()],
            Instruction::Unreachable => vec![WasmInstruction::Unreachable],
        }
    }

    fn lower_call(&self, ident: &str) -> WasmInstruction {
        match ident {
            // Arithmetic Operators
            "add__s32_s32" | "add__u32_u32" => WasmInstruction::I32Add,
            "add__s64_s64" | "add__u64_u64" => WasmInstruction::I64Add,
            "add__f32_f32" => WasmInstruction::F32Add,
            "add__f64_f64" => WasmInstruction::F64Add,

            "sub__s32_s32" | "sub__u32_u32" => WasmInstruction::I32Sub,
            "sub__s64_s64" | "sub__u64_u64" => WasmInstruction::I64Sub,
            "sub__f32_f32" => WasmInstruction::F32Sub,
            "sub__f64_f64" => WasmInstruction::F64Sub,

            "negate__f32" => WasmInstruction::F32Neg,
            "negate__f64" => WasmInstruction::F64Neg,

            "mul__s32_s32" | "mul__u32_u32" => WasmInstruction::I32Mul,
            "mul__s64_s64" | "mul__u64_u64" => WasmInstruction::I64Mul,
            "mul__f32_f32" => WasmInstruction::F32Mul,
            "mul__f64_f64" => WasmInstruction::F64Mul,

            "div__u32_u32" => WasmInstruction::I32DivU,
            "div__u64_u64" => WasmInstruction::I64DivU,
            "div__s32_s32" => WasmInstruction::I32DivS,
            "div__s64_s64" => WasmInstruction::I64DivS,
            "div__f32_f32" => WasmInstruction::F32Div,
            "div__f64_f64" => WasmInstruction::F64Div,

            // Comparison Operators
            "eq__u32_u32" | "eq__s32_s32" => WasmInstruction::I32Eq,
            "eq__u64_u64" | "eq__s64_s64" => WasmInstruction::I64Eq,
            "eq__f32" => WasmInstruction::F32Eq,
            "eq__f64" => WasmInstruction::F64Eq,

            "ne__u32_u32" | "ne__s32_s32" => WasmInstruction::I32Ne,
            "ne__u64_u64" | "ne__s64_s64" => WasmInstruction::I64Ne,
            "ne__f32" => WasmInstruction::F32Ne,
            "ne__f64" => WasmInstruction::F64Ne,

            "greater_than__u32_u32" => WasmInstruction::I32GtU,
            "greater_than__u64_u64" => WasmInstruction::I64GtU,
            "greater_than__s32_s32" => WasmInstruction::I32GtS,
            "greater_than__s64_s64" => WasmInstruction::I64GtS,
            "greater_than__f32_f32" => WasmInstruction::F32Gt,
            "greater_than__f64_f64" => WasmInstruction::F64Gt,

            "less_than__u32_u32" => WasmInstruction::I32LtU,
            "less_than__u64_u64" => WasmInstruction::I64LtU,
            "less_than__s32_s32" => WasmInstruction::I32LtS,
            "less_than__s64_s64" => WasmInstruction::I64LtS,
            "less_than__f32_f32" => WasmInstruction::F32Lt,
            "less_than__f64_f64" => WasmInstruction::F64Lt,

            "greater_eq__u32_u32" => WasmInstruction::I32GeU,
            "greater_eq__u64_u64" => WasmInstruction::I64GeU,
            "greater_eq__s32_s32" => WasmInstruction::I32GeS,
            "greater_eq__s64_s64" => WasmInstruction::I64GeS,
            "greater_eq__f32_f32" => WasmInstruction::F32Ge,
            "greater_eq__f64_f64" => WasmInstruction::F64Ge,

            "less_eq__u32_u32" => WasmInstruction::I32LeU,
            "less_eq__u64_u64" => WasmInstruction::I64LeU,
            "less_eq__s32_s32" => WasmInstruction::I32LeS,
            "less_eq__s64_s64" => WasmInstruction::I64LeS,
            "less_eq__f32_f32" => WasmInstruction::F32Le,
            "less_eq__f64_f64" => WasmInstruction::F64Le,

            "min__f32_f32" => WasmInstruction::F32Min,
            "min__f64_f64" => WasmInstruction::F64Min,

            "max__f32_f32" => WasmInstruction::F32Max,
            "max__f64_f64" => WasmInstruction::F64Max,

            // Unary float operations
            "floor__f64" => WasmInstruction::F64Floor,
            "floor__f32" => WasmInstruction::F32Floor,

            "ceil__f64" => WasmInstruction::F64Ceil,
            "ceil__f32" => WasmInstruction::F32Ceil,

            "abs__f32" => WasmInstruction::F32Abs,
            "abs__f64" => WasmInstruction::F64Abs,

            // Conversions
            "s32__f32" => WasmInstruction::I32TruncF32S,
            "s64__f32" => WasmInstruction::I64TruncF32S,
            "u32__f32" => WasmInstruction::I32TruncF32U,
            "u64__f32" => WasmInstruction::I64TruncF32U,

            "s32__f64" => WasmInstruction::I32TruncF64S,
            "s64__f64" => WasmInstruction::I64TruncF64S,
            "u32__f64" => WasmInstruction::I32TruncF64U,
            "u64__f64" => WasmInstruction::I64TruncF64U,

            // Regular function call
            ident => {
                let ident = ident.replace("_", "-");

                let index = self
                    .functions
                    .iter()
                    .enumerate()
                    .find(|(_, f)| f.name == ident)
                    .map(|(idx, _)| idx)
                    .unwrap_or_else(|| {
                        panic!(
                            "Function '{ident}' should exist, got: {:#?}",
                            self.functions
                        )
                    });

                WasmInstruction::Call(index as u32)
            }
        }
    }

    fn enumerate_locals(&self, locals: &[TypeRef]) -> Vec<Local> {
        let mut offset = 0;
        let mut result = Vec::new();

        for ty in locals {
            let lower = self.lower(ty);
            let size = lower.len() as u32;

            result.push(Local {
                index: offset,
                ty: ty.clone(),
                lower,
            });
            offset += size;
        }

        result
    }
}

fn lower_types(types: &[TypeDeclaration]) -> Vec<(Type, Vec<ValType>)> {
    let mut result: Vec<Option<Vec<ValType>>> = types.iter().map(|_| None).collect();

    for (index, ty) in types.iter().enumerate() {
        if result[index].is_some() {
            continue;
        }

        result[index] = Some(lower_type_update(
            &ty.ty,
            &types.iter().map(|decl| decl.ty.clone()).collect::<Vec<_>>(),
            &mut result,
        ));
    }

    let mapping = result.into_iter().map(|lowered| lowered.unwrap());

    types
        .into_iter()
        .map(|decl| decl.ty.clone())
        .zip(mapping)
        .collect()
}

fn lower_locals<'a>(
    locals: impl Iterator<Item = &'a Local>,
) -> impl Iterator<Item = (u32, ValType)> {
    locals.flat_map(lower_local)
}

fn lower_local(local: &Local) -> Vec<(u32, ValType)> {
    local
        .lower
        .iter()
        .enumerate()
        .map(|(i, ty)| (local.index + i as u32, ty.clone()))
        .collect::<Vec<_>>()
}

fn lower_type_update(
    ty: &Type,
    types: &[Type],
    mapping: &mut [Option<Vec<ValType>>],
) -> Vec<ValType> {
    match ty {
        Type::Simple(t) => match t {
            TypeRef::Primitive(p) => p.lower(),
            TypeRef::Type(i) => {
                let i = *i as usize;
                let mapped = &mapping[i];
                match &mapped {
                    Some(types) => types.clone(),
                    None => {
                        let types = lower_type_update(&types[i], &types, mapping);
                        mapping[i] = Some(types.clone());

                        types
                    }
                }
            }
        },
        Type::List(_) => vec![ValType::I32, ValType::I32],
        Type::Option(inner) => {
            let inner = lower_type_update(&inner.into(), types, mapping);

            // Discriment (i32) + lowered payload
            [vec![ValType::I32], inner].concat()
        }
        Type::Result { ok, err } => {
            let ok = lower_type_update(&ok.into(), types, mapping);
            let err = lower_type_update(&err.into(), types, mapping);

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
            .flat_map(|(_, ty)| lower_type_update(&ty.into(), types, mapping))
            .collect(),
        Type::Variant {} => todo!(),
        Type::Tuple(fields) => fields
            .into_iter()
            .flat_map(|ty| lower_type_update(&ty.into(), types, mapping))
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

// Type of branch label
enum BranchLabel {
    Block,
    If,
    Loop,
    // Special block that the compiler inserts to allow breaking out of loops
    LoopBlock,
}
