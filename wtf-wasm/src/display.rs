use std::fmt::{Formatter, Result};

use wasm_encoder::ComponentValType;

use crate::{Instance, Instruction, Type};

const WHITESPACE: &str = "  ";

pub trait Print {
    fn print(&self, f: &mut Formatter<'_>, indent: usize) -> Result;
}

impl Print for Instance<'_> {
    fn print(&self, f: &mut Formatter<'_>, _indent: usize) -> Result {
        for (i, ty) in self.types.iter().enumerate() {
            write!(f, "{i:>3}: ")?;
            ty.print(f, 0)?;
        }
        writeln!(f, "")?;

        for func in &self.functions {
            writeln!(f, "function {}", func.signature.name)?;

            if !func.locals.is_empty() {
                write!(f, "(locals:")?;
                for (i, local) in func.locals.iter().enumerate() {
                    write!(f, " ")?;
                    local.print(f, 0)?;
                    if i < func.locals.len() - 1 {
                        write!(f, ",")?;
                    }
                }
                writeln!(f, ")")?;
            }

            for inst in &func.instructions {
                inst.print(f, 1)?;
            }
            writeln!(f, "")?;
        }

        Ok(())
    }
}

impl Print for Instruction<'_> {
    fn print(&self, f: &mut Formatter<'_>, indent: usize) -> Result {
        for _ in 0..indent {
            write!(f, "{}", WHITESPACE)?;
        }

        match self {
            Instruction::LocalGet(id) => writeln!(f, "get: {id}")?,
            Instruction::LocalSet(id) => writeln!(f, "set: {id}")?,
            Instruction::Call(name) => writeln!(f, "call: {name}")?,
            Instruction::LocalGetMember { id, member } => todo!(),
            Instruction::Store { number, ty } => writeln!(f, "store: {number}")?,
            Instruction::Bytes(items) => {
                writeln!(
                    f,
                    "bytes: {}",
                    items
                        .iter()
                        .map(|i| i.to_string())
                        .collect::<Vec<_>>()
                        .join(", ")
                )?;
            }
            Instruction::IndexAccess { ty: _ } => writeln!(f, "index")?,
            Instruction::Optional { ty: _, is_some } => writeln!(f, "optional: {is_some}")?,
            Instruction::I32(val) => writeln!(f, "i32: {val}")?,
            Instruction::I64(val) => writeln!(f, "i64: {val}")?,
            Instruction::F32(val) => writeln!(f, "f32: {val}")?,
            Instruction::F64(val) => writeln!(f, "f64: {val}")?,
            Instruction::If { then, else_ } => {
                writeln!(f, "if:")?;
                for inst in then {
                    inst.print(f, indent + 1)?;
                }

                if !else_.is_empty() {
                    for _ in 0..indent {
                        write!(f, "{}", WHITESPACE)?;
                    }
                    writeln!(f, "else:")?;
                    for inst in else_ {
                        inst.print(f, indent + 1)?;
                    }
                }
            }
            Instruction::Loop(instructions) => {
                writeln!(f, "loop:")?;
                for inst in instructions {
                    inst.print(f, indent + 1)?;
                }
            }
            Instruction::Block => todo!(),
            Instruction::Break => writeln!(f, "break")?,
            Instruction::Continue => writeln!(f, "continue")?,
            Instruction::Return => writeln!(f, "return")?,
            Instruction::End => writeln!(f, "end")?,
            Instruction::Unreachable => writeln!(f, "unreachable")?,
            Instruction::Pop => writeln!(f, "pop")?,
            Instruction::Noop => writeln!(f, "noop")?,
            Instruction::Wasm(instruction) => {
                writeln!(f, "wasm: {:#?}", instruction)?;
            }
        };

        Ok(())
    }
}

impl Print for ComponentValType {
    fn print(&self, f: &mut Formatter<'_>, _indent: usize) -> Result {
        match self {
            ComponentValType::Primitive(ty) => {
                let repr = match ty {
                    wasm_encoder::PrimitiveValType::Bool => "bool",
                    wasm_encoder::PrimitiveValType::S8 => "s8",
                    wasm_encoder::PrimitiveValType::U8 => "u8",
                    wasm_encoder::PrimitiveValType::S16 => "s16",
                    wasm_encoder::PrimitiveValType::U16 => "u16",
                    wasm_encoder::PrimitiveValType::S32 => "s32",
                    wasm_encoder::PrimitiveValType::U32 => "u32",
                    wasm_encoder::PrimitiveValType::S64 => "s64",
                    wasm_encoder::PrimitiveValType::U64 => "u64",
                    wasm_encoder::PrimitiveValType::F32 => "f32",
                    wasm_encoder::PrimitiveValType::F64 => "f64",
                    wasm_encoder::PrimitiveValType::Char => "char",
                    wasm_encoder::PrimitiveValType::String => "string",
                };
                write!(f, "{repr}")
            }
            ComponentValType::Type(id) => {
                write!(f, "@{id}")
            }
        }
    }
}

impl Print for Type {
    fn print(&self, f: &mut Formatter<'_>, indent: usize) -> Result {
        for _ in 0..indent {
            write!(f, "{}", WHITESPACE)?;
        }

        match self {
            Type::Simple(ty) => ty.print(f, indent)?,
            Type::List(elem) => {
                write!(f, "[")?;
                elem.print(f, indent)?;
                write!(f, "]")?;
            }
            Type::Option(some) => {
                some.print(f, indent)?;
                write!(f, "?")?;
            }
            Type::Result { ok, err } => todo!(),
            Type::Record { fields } => todo!(),
            Type::Variant {} => todo!(),
            Type::Tuple(component_val_types) => todo!(),
            Type::Flags(items) => todo!(),
            Type::Enum(items) => todo!(),
            Type::Own() => todo!(),
            Type::Borrow() => todo!(),
        }

        writeln!(f, "")
    }
}
