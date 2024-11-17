use std::{fmt::Debug, iter};

use wtf_hir as hir;
use wtf_wasm::{
    ComponentBuilder, Function, Instance, Instruction, PrimitiveType, Signature, Type,
    TypeDeclaration, TypeRef,
};

// PERF: use hash map here if search turns out to be slow
#[derive(Debug, Default)]
struct TypeLookup(Vec<Type>);

impl TypeLookup {
    /// Adds the type if it does not exist yet and returns its index afterwards
    fn insert(&mut self, ty: Type) -> u32 {
        let found = self.0.iter().position(|t| *t == ty);
        let idx = match found {
            Some(i) => i,
            None => {
                self.0.push(ty);
                self.0.len() - 1
            }
        };

        idx as u32
    }
}

#[derive(Debug, Default)]
pub struct Encoder {
    // Lookup of all internal type definitions to their index
    types: TypeLookup,
    builder: ComponentBuilder,
}

impl Encoder {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn encode(mut self, module: hir::Module) -> Encoder {
        let instance = module.convert(&mut self.types);
        self.builder.encode_instance(instance);

        self
    }

    pub fn finish(self) -> Vec<u8> {
        self.builder.finish()
    }
}

trait Convert<'a> {
    type Output;

    fn convert(self, lookup: &mut TypeLookup) -> Self::Output;
}

impl<'a> Convert<'a> for hir::Module {
    type Output = Instance<'a>;

    fn convert(self, lookup: &mut TypeLookup) -> Self::Output {
        let types = self.types.into_iter().map(|t| t.convert(lookup)).collect();
        let functions = self
            .functions
            .into_iter()
            .map(|t| t.convert(lookup))
            .collect();

        Instance {
            name: "todo".to_owned(),
            functions,
            types,
        }
    }
}

impl Convert<'_> for (String, hir::Type) {
    type Output = TypeDeclaration;

    fn convert(self, lookup: &mut TypeLookup) -> Self::Output {
        let (name, ty) = self;
        let ty = match ty {
            hir::Type::Never => todo!(),
            hir::Type::None => todo!(),
            hir::Type::List(_) => todo!(),
            hir::Type::Option(_) => todo!(),
            hir::Type::Result { ok, err } => todo!(),
            hir::Type::Record(rec) => Type::Record {
                fields: rec
                    .into_iter()
                    .map(|(k, v)| (k, v.convert(lookup).unwrap()))
                    .collect(),
            },
            hir::Type::Resource(_) => todo!(),
            hir::Type::Enum(_) => todo!(),
            hir::Type::Variant(_) => todo!(),
            hir::Type::Tuple(_) => todo!(),
            hir::Type::Builtin(_) => todo!(),
        };

        // TODO: Preserve export information
        TypeDeclaration {
            name,
            ty,
            export: true,
        }
    }
}

impl<'a> Convert<'a> for (String, hir::Function) {
    type Output = Function<'a>;

    fn convert(self, lookup: &mut TypeLookup) -> Self::Output {
        let (name, func) = self;
        let params: Vec<_> = func
            .parameters
            .into_iter()
            .map(|(name, ty)| {
                (
                    name,
                    ty.convert(lookup)
                        .expect("Parameter type must be a valid type"),
                )
            })
            .collect();
        let result = func.return_type.convert(lookup);
        let instructions: Vec<Instruction> = func
            .body
            .instructions
            .into_iter()
            .map(|exp| exp.convert(lookup))
            .chain(iter::once(Instruction::End))
            .collect();
        let locals = func
            .locals
            .into_iter()
            .map(|ty| {
                ty.convert(lookup)
                    .expect("Type of local must be defined before use")
            })
            .collect();
        let func = Function {
            signature: Signature {
                params,
                result,
                name: name.replace("_", "-"),
                export: func.is_export,
            },
            instructions,
            locals,
        };

        func
    }
}

impl<'a> Convert<'a> for hir::Instruction {
    type Output = Instruction<'a>;

    fn convert(self, lookup: &mut TypeLookup) -> Self::Output {
        match self {
            hir::Instruction::Pop => todo!(),
            hir::Instruction::Load(i) => Instruction::LocalGet(i.0 as u32),
            hir::Instruction::Store(i) => Instruction::LocalSet(i.0 as u32),
            hir::Instruction::Int(num) => Instruction::Int(num),
            hir::Instruction::Float(_) => todo!(),
            hir::Instruction::String(_) => todo!(),
            hir::Instruction::Bool(_) => todo!(),
            hir::Instruction::None => Instruction::Noop,
            hir::Instruction::Enum {
                variant,
                num_payloads,
            } => todo!(),
            hir::Instruction::Record(_) => todo!(),
            hir::Instruction::List(_) => todo!(),
            hir::Instruction::Call {
                function,
                num_arguments,
            } => Instruction::Call(function),
            hir::Instruction::FieldAccess(field) => {
                todo!("Lower field access to wasm (lookup offset of parent variable)")
            }
            hir::Instruction::IndexAccess => todo!(),
            hir::Instruction::Return => Instruction::Return,
            hir::Instruction::Break => todo!(),
            hir::Instruction::Continue => todo!(),
            hir::Instruction::Throw => todo!(),
            hir::Instruction::If { then, else_ } => Instruction::If {
                then: then.convert(lookup),
                else_: else_.convert(lookup),
            },
            hir::Instruction::Match { arms } => todo!(),
            hir::Instruction::Loop(_) => todo!(),
            hir::Instruction::Unreachable => Instruction::Unreachable,
        }
    }
}

impl<'a> Convert<'a> for hir::Block {
    type Output = Vec<Instruction<'a>>;

    fn convert(self, lookup: &mut TypeLookup) -> Self::Output {
        self.instructions
            .into_iter()
            .map(|inst| inst.convert(lookup))
            .collect()
    }
}

impl Convert<'_> for hir::Type {
    type Output = Option<TypeRef>;

    fn convert(self, lookup: &mut TypeLookup) -> Self::Output {
        match self {
            hir::Type::List(item) => {
                let item = item.convert(lookup);
                let ty = lookup.insert(Type::List(item.expect("Lists must have an inner type")));

                Some(TypeRef::Type(ty))
            }
            hir::Type::Option(inner) => {
                let inner = inner.convert(lookup);
                let ty = lookup.insert(Type::Option(
                    inner.expect("Options must have an inner type"),
                ));

                Some(TypeRef::Type(ty))
            }
            hir::Type::Result { ok, err } => {
                let ok = ok
                    .convert(lookup)
                    .expect("Result 'ok' must be a valid type");
                let err = err
                    .convert(lookup)
                    .expect("Result errors must be a valid type");
                let ty = lookup.insert(Type::Result { ok, err });

                Some(TypeRef::Type(ty))
            }
            hir::Type::Record(fields) => {
                let record = Type::Record {
                    fields: fields
                        .into_iter()
                        .map(|(k, v)| (k, v.convert(lookup).unwrap()))
                        .collect(),
                };
                let ty = lookup.insert(record);

                Some(TypeRef::Type(ty))
            }
            hir::Type::Resource(_) => todo!(),
            hir::Type::Enum(cases) => {
                let ty = lookup.insert(Type::Enum(cases));

                Some(TypeRef::Type(ty))
            }
            hir::Type::Variant(_) => todo!(),
            hir::Type::Tuple(_) => todo!(),
            hir::Type::Builtin(ty) => Some(TypeRef::Primitive(ty.convert(lookup))),
            hir::Type::None => None,
            hir::Type::Never => todo!("Disallow 'Never' for exported functions"),
        }
    }
}

impl Convert<'_> for hir::PrimitiveType {
    type Output = PrimitiveType;

    fn convert(self, _lookup: &mut TypeLookup) -> Self::Output {
        match self {
            hir::PrimitiveType::Bool => PrimitiveType::Bool,
            hir::PrimitiveType::S8 => PrimitiveType::S8,
            hir::PrimitiveType::U8 => PrimitiveType::U8,
            hir::PrimitiveType::S16 => PrimitiveType::S16,
            hir::PrimitiveType::U16 => PrimitiveType::U16,
            hir::PrimitiveType::S32 => PrimitiveType::S32,
            hir::PrimitiveType::U32 => PrimitiveType::U32,
            hir::PrimitiveType::S64 => PrimitiveType::S64,
            hir::PrimitiveType::U64 => PrimitiveType::U64,
            hir::PrimitiveType::F32 => PrimitiveType::F32,
            hir::PrimitiveType::F64 => PrimitiveType::F64,
            hir::PrimitiveType::Char => PrimitiveType::Char,
            hir::PrimitiveType::String => PrimitiveType::String,
        }
    }
}
