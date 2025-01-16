use wasm_encoder::{ComponentValType, Instruction as WasmInstruction};

/// High level instruction that can handle high-level types
#[derive(Debug, Clone)]
pub enum Instruction<'a> {
    // Locals
    LocalGet(u32),
    LocalSet(u32),
    Call(String),

    LocalGetMember {
        id: u32,
        /// Index of the field
        member: Vec<u32>,
    },

    Store {
        number: usize,
        ty: ComponentValType,
    },

    Bytes(Vec<u8>),
    IndexAccess {
        ty: ComponentValType,
    },

    I32(i32),
    I64(i64),
    F32(f32),
    F64(f64),

    // Control Flow
    If {
        then: Vec<Instruction<'a>>,
        else_: Vec<Instruction<'a>>,
    },
    Loop(Vec<Instruction<'a>>),
    Block,
    Break,
    Continue,
    Return,
    End,
    Unreachable,
    Pop,
    Noop,

    // Raw WASM instructions to allow writing inline WASM functions directly in the future
    Wasm(WasmInstruction<'a>),
}

impl<'a> From<WasmInstruction<'a>> for Instruction<'a> {
    fn from(value: WasmInstruction<'a>) -> Self {
        Self::Wasm(value)
    }
}

pub trait InstructionVec<'a> {
    fn into_instructions(self) -> Vec<Instruction<'a>>;
}

impl<'a> InstructionVec<'a> for Vec<WasmInstruction<'a>> {
    fn into_instructions(self) -> Vec<Instruction<'a>> {
        self.into_iter().map(Into::into).collect()
    }
}
