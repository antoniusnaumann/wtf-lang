use wasm_encoder::Instruction as WasmInstruction;

use crate::Type;

/// High level instruction that can handle high-level types
#[derive(Debug, Clone)]
pub enum Instruction<'a> {
    // Locals
    LocalGet(String),
    LocalSet(String),
    Call(String),

    // TODO: Add consts
    Const,

    // Control Flow
    If,
    Else,
    Loop,
    Block,
    Branch,
    BranchIf,
    Return,
    End,

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
