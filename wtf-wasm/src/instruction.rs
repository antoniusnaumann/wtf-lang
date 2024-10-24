use wasm_encoder::Instruction as WasmInstruction;

use crate::Type;

/// High level instruction that can handle high-level types
#[derive(Debug, Clone)]
pub enum Instruction<'a> {
    // Locals
    LocalGet(u32),
    LocalSet(u32),
    Call(String),

    // TODO: Add consts
    Const,
    Int(i64),

    // Control Flow
    If {
        then: Vec<Instruction<'a>>,
        else_: Vec<Instruction<'a>>,
    },
    Else,
    Loop,
    Block,
    Branch,
    BranchIf,
    Return,
    End,
    Unreachable,

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
