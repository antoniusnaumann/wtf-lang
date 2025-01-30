use std::{collections::HashSet, fmt::Debug, iter};

use wtf_hir::{self as hir, Test};
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
    builder: ComponentBuilder,
}

impl Encoder {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn encode(mut self, module: hir::Module) -> Encoder {
        let instance = module.convert();
        self.builder.encode_instance(instance);

        self
    }

    pub fn finish(self) -> Vec<u8> {
        self.builder.finish()
    }
}

trait ConvertModule<'a> {
    type Output;

    fn convert(self) -> Self::Output;
}

trait Convert<'a> {
    type Output;

    fn convert(self, lookup: &mut TypeLookup) -> Self::Output;
}

impl<'a> ConvertModule<'a> for hir::Module {
    type Output = Instance<'a>;

    fn convert(self) -> Self::Output {
        let mut lookup = TypeLookup::default();
        for ty in self.types {
            ty.convert(&mut lookup);
        }
        let tests = self
            .tests
            .into_iter()
            .map(|t| t.convert(&mut lookup))
            .collect::<Vec<_>>();
        let functions = self
            .functions
            .into_iter()
            .map(|t| t.convert(&mut lookup))
            .chain(tests)
            .collect();

        Instance {
            name: "todo".to_owned(),
            functions,
            // TODO: Avoid cloning here
            types: lookup.0,
            constants: self.constants,
        }
    }
}

impl Convert<'_> for (String, hir::Type) {
    type Output = TypeDeclaration;

    fn convert(self, lookup: &mut TypeLookup) -> Self::Output {
        let (name, ty) = self;
        let ty = match ty {
            hir::Type::Never => todo!(),
            hir::Type::Void => todo!(),
            hir::Type::Blank => todo!(),
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
            hir::Type::Enum(ty) => Type::Enum(ty.cases),
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
        let locals = convert_locals(func.locals, lookup);
        let instructions = convert_body(func.body, lookup, &locals);
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

impl<'a> Convert<'a> for Test {
    type Output = Function<'a>;

    fn convert(self, lookup: &mut TypeLookup) -> Self::Output {
        let locals = convert_locals(self.locals, lookup);
        let instructions = convert_body(self.body, lookup, &locals);

        Function {
            signature: Signature {
                params: vec![],
                result: None,
                name: self.id,
                export: true,
            },
            locals,
            instructions,
        }
    }
}

fn convert_locals(locals: Vec<hir::Type>, lookup: &mut TypeLookup) -> Vec<TypeRef> {
    locals
        .into_iter()
        .map(|ty| {
            ty.convert(lookup)
                .expect("Type of local must be defined before use")
        })
        .collect()
}

fn convert_body<'a>(
    body: hir::Block,
    lookup: &mut TypeLookup,
    locals: &[TypeRef],
) -> Vec<Instruction<'a>> {
    body.instructions
        .into_iter()
        .map(|exp| exp.convert(lookup, &locals))
        .chain(iter::once(Instruction::End))
        .collect()
}

trait ConvertInstruction<'a> {
    type Output;

    fn convert(self, lookup: &mut TypeLookup, locals: &[TypeRef]) -> Self::Output;
}

impl<'a> ConvertInstruction<'a> for hir::Block {
    type Output = Vec<Instruction<'a>>;

    fn convert(self, lookup: &mut TypeLookup, locals: &[TypeRef]) -> Self::Output {
        self.instructions
            .into_iter()
            .map(|inst| inst.convert(lookup, locals))
            .collect()
    }
}

impl<'a> ConvertInstruction<'a> for hir::Instruction {
    type Output = Instruction<'a>;

    fn convert(self, lookup: &mut TypeLookup, locals: &[TypeRef]) -> Self::Output {
        match self {
            hir::Instruction::Pop => Instruction::Pop,
            hir::Instruction::Load(i) => Instruction::LocalGet(i.0 as u32),
            hir::Instruction::Store(i) => Instruction::LocalSet(i.0 as u32),
            hir::Instruction::Int(num) => Instruction::I32(num as i32), // TODO typecheck: different instructions for different int sizes
            hir::Instruction::Float(num) => Instruction::F32(num as f32), // TODO: typecheck: different instructions for different float sizes
            hir::Instruction::String(string) => Instruction::Bytes(string.into_bytes()),
            hir::Instruction::Bool(b) => Instruction::I32(if b { 1 } else { 0 }),
            hir::Instruction::Void => Instruction::Noop,
            hir::Instruction::Enum { case, ty: _ } => Instruction::I32(case as i32), // TODO: if the enum type has more fields than i32 allows, use i64
            hir::Instruction::Variant { case, num_payloads } => todo!(),
            hir::Instruction::Record(_) => Instruction::Noop,
            hir::Instruction::Option { is_some, ty } => todo!("Convert optional to instructions"),
            hir::Instruction::List { len, ty } => {
                let ty = ty.convert(lookup).expect("List elements must have a type");
                Instruction::Store { number: len, ty }
            }
            hir::Instruction::Call {
                function,
                num_arguments,
            } => Instruction::Call(function),
            hir::Instruction::MemberChain(id, fields) => {
                let mut current = &locals[id.0];
                let mut member = vec![];
                for field in fields {
                    let record = match current {
                        TypeRef::Primitive(_) => panic!("Cannot access member of a primitive"),
                        TypeRef::Type(ty) => &lookup.0[*ty as usize],
                    };
                    let Type::Record { fields } = record else {
                        panic!("ERROR: can only access fields of records");
                    };

                    let (index, ty) = fields
                        .iter()
                        .enumerate()
                        .find_map(
                            |(i, (name, ty))| {
                                if *name == field {
                                    Some((i, ty))
                                } else {
                                    None
                                }
                            },
                        )
                        .expect("Accessed field did not exist");

                    member.push(index as u32);
                    current = ty;
                }

                Instruction::LocalGetMember {
                    id: id.0 as u32,
                    // TODO: convert field names to indices
                    member,
                }
            }
            hir::Instruction::FieldAccess(field) => {
                todo!("look at type here and figure out what to pop?")
            }
            hir::Instruction::IndexAccess { ty } => {
                let ty = ty.convert(lookup).expect("List elements must have a type");

                Instruction::IndexAccess { ty }
            }
            hir::Instruction::Return => Instruction::Return,
            hir::Instruction::Break => Instruction::Break,
            hir::Instruction::Continue => Instruction::Continue,
            hir::Instruction::Throw => todo!(),
            hir::Instruction::If { then, else_ } => Instruction::If {
                then: then.convert(lookup, locals),
                else_: else_.convert(lookup, locals),
            },
            hir::Instruction::Match { arms } => todo!(),
            hir::Instruction::Loop(block) => Instruction::Loop(block.convert(lookup, locals)),
            hir::Instruction::Unreachable => Instruction::Unreachable,
        }
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
            hir::Type::Enum(ty) => {
                let ty = lookup.insert(Type::Enum(ty.cases));

                Some(TypeRef::Type(ty))
            }
            hir::Type::Variant(_) => todo!(),
            hir::Type::Tuple(_) => todo!(),
            hir::Type::Builtin(ty) => Some(TypeRef::Primitive(ty.convert(lookup))),
            hir::Type::Void => None,
            hir::Type::Never => todo!("Disallow 'Never' for exported functions"),
            hir::Type::Blank => todo!("'Blank' not allowed here"),
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
