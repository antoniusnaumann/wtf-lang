use std::{collections::HashSet, fmt::Debug, ops::Sub};

use wtf_hir::{self as hir, Expression, Test};
use wtf_wasm::{
    ComponentBuilder, Function, Instance, Instruction, PrimitiveType, Signature, Type,
    TypeDeclaration, TypeRef,
};

// PERF: use hash map here if search turns out to be slow
#[derive(Debug, Default)]
struct Lookup {
    types: Vec<Type>,
    constants: HashSet<Vec<u8>>,
}

impl Lookup {
    /// Adds the type if it does not exist yet and returns its index afterwards
    fn insert(&mut self, ty: Type) -> u32 {
        let found = self.types.iter().position(|t| *t == ty);
        let idx = match found {
            Some(i) => i,
            None => {
                self.types.push(ty);
                self.types.len() - 1
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

    pub fn encode(mut self, instance: Instance) -> Encoder {
        self.builder.encode_instance(instance);

        self
    }

    pub fn finish(self) -> Vec<u8> {
        self.builder.finish()
    }
}

pub fn compile<'a>(hir: hir::Module) -> Instance<'a> {
    hir.convert()
}

trait ConvertModule<'a> {
    type Output;

    fn convert(self) -> Self::Output;
}

trait Convert<'a> {
    type Output;

    fn convert(self, lookup: &mut Lookup) -> Self::Output;
}

impl<'a> ConvertModule<'a> for hir::Module {
    type Output = Instance<'a>;

    fn convert(self) -> Self::Output {
        let mut lookup = Lookup::default();
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
            types: lookup.types,
            constants: lookup.constants,
        }
    }
}

impl Convert<'_> for (String, hir::Type) {
    type Output = TypeDeclaration;

    fn convert(self, lookup: &mut Lookup) -> Self::Output {
        let (name, ty) = self;
        let ty = match ty {
            hir::Type::Never => todo!(),
            hir::Type::None => todo!(),
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
            hir::Type::Tuple(_) => todo!(),
            hir::Type::Bool => todo!(),
            hir::Type::Char => todo!(),
            hir::Type::Int { signed, bits } => todo!(),
            hir::Type::Float { bits } => todo!(),
            hir::Type::String => todo!(),
            hir::Type::Resource { methods } => todo!(),
            hir::Type::Enum { cases } => Type::Enum(cases),
            hir::Type::Variant { cases } => todo!(),
            hir::Type::Meta(_) => todo!(),
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

    fn convert(self, lookup: &mut Lookup) -> Self::Output {
        let (name, func) = self;
        let params: Vec<_> = func
            .parameters
            .into_iter()
            .map(|param| {
                (
                    param.name,
                    param
                        .ty
                        .convert(lookup)
                        .expect("Parameter type must be a valid type"),
                )
            })
            .collect();
        let result = func.return_type.convert(lookup);
        let mut locals = convert_locals(func.body.vars.clone(), lookup);
        let instructions = convert_function_body(func.body, lookup, &mut locals);

        Function {
            signature: Signature {
                params,
                result,
                name: name.replace("_", "-"),
                export: func.is_export,
            },
            instructions,
            locals,
        }
    }
}

impl<'a> Convert<'a> for Test {
    type Output = Function<'a>;

    fn convert(self, lookup: &mut Lookup) -> Self::Output {
        let mut locals = convert_locals(self.body.vars.clone(), lookup);
        let instructions = convert_function_body(self.body, lookup, &mut locals);

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

fn convert_locals(locals: Vec<hir::Type>, lookup: &mut Lookup) -> Vec<TypeRef> {
    locals
        .into_iter()
        .map(|ty| {
            ty.convert(lookup)
                .expect("Type of local must be defined before use")
        })
        .collect()
}

fn convert_function_body<'a, 'temp>(
    body: hir::FunctionBody,
    lookup: &'temp mut Lookup,
    locals: &'temp mut Vec<TypeRef>,
) -> Vec<Instruction<'a>>
where
    'a: 'temp,
{
    body.body
        .statements
        .into_iter()
        .flat_map(|exp| exp.convert(lookup, locals))
        .collect()
}

trait ConvertInstruction<'a, 'temp>
where
    'a: 'temp,
{
    type Output;

    fn convert(self, lookup: &'temp mut Lookup, locals: &'temp mut Vec<TypeRef>) -> Self::Output;
}

impl<'a, 'temp> ConvertInstruction<'a, 'temp> for hir::Body
where
    'a: 'temp,
{
    type Output = Vec<Instruction<'a>>;

    fn convert(self, lookup: &'temp mut Lookup, locals: &'temp mut Vec<TypeRef>) -> Self::Output {
        self.statements
            .into_iter()
            .flat_map(|statement| statement.to_owned().convert(lookup, locals))
            .collect()
    }
}

impl<'a, 'temp> ConvertInstruction<'a, 'temp> for hir::Expression
where
    'a: 'temp,
{
    type Output = Vec<Instruction<'a>>;

    fn convert(self, lookup: &'temp mut Lookup, locals: &'temp mut Vec<TypeRef>) -> Self::Output {
        let mut builder = InstructionBuilder::new(lookup, locals);
        builder.push(self);
        builder.instructions
    }
}

struct InstructionBuilder<'a, 'temp> {
    instructions: Vec<Instruction<'a>>,
    lookup: &'temp mut Lookup,
    locals: &'temp mut Vec<TypeRef>,
}

impl<'temp> InstructionBuilder<'_, 'temp> {
    fn new(lookup: &'temp mut Lookup, locals: &'temp mut Vec<TypeRef>) -> Self {
        Self {
            instructions: Vec::new(),
            lookup,
            locals,
        }
    }

    fn push(&mut self, expression: Expression) {
        let instruction = match expression.kind {
            hir::ExpressionKind::Int(num) => Instruction::I32(num as i32), // TODO typecheck: different instructions for different int sizes
            hir::ExpressionKind::Float(num) => Instruction::F32(num as f32), // TODO: typecheck: different instructions for different float sizes
            hir::ExpressionKind::String(string) => {
                let bytes = string.into_bytes();
                self.lookup.constants.insert(bytes.clone());
                // TODO: "bytes" should probably reference an index or so instead of containing this directly
                Instruction::Bytes(bytes)
            }
            hir::ExpressionKind::Bool(b) => Instruction::I32(if b { 1 } else { 0 }),
            hir::ExpressionKind::Enum { case } => Instruction::I32(case as i32), // TODO: if the enum type has more fields than i32 allows, use i64
            hir::ExpressionKind::Variant { case, payloads } => todo!(),
            hir::ExpressionKind::Record(fields) => {
                for (_, field) in fields {
                    self.push(field);
                }
                return;
            }
            hir::ExpressionKind::Option(option) => {
                let hir::Type::Option(ty) = expression.ty.clone() else {
                    panic!("Expected option type!");
                };

                let is_some = option.is_some();
                self.push(hir::ExpressionKind::Bool(is_some).typed(*ty.clone()));
                match option {
                    Some(inner) => self.push(*inner),
                    None => self.push(hir::ExpressionKind::Zero.typed(*ty)),
                }
                return;
            }
            hir::ExpressionKind::List(items) => {
                let hir::Type::List(ty) = expression.ty.clone() else {
                    panic!("Expected list type!");
                };

                let ty = ty
                    .convert(self.lookup)
                    .expect("List elements must have a type");

                let number = items.len();

                for item in items {
                    self.push(item);
                }

                Instruction::Store { number, ty }
            }
            hir::ExpressionKind::Call {
                function,
                arguments,
            } => {
                for arg in arguments.clone() {
                    self.push(arg)
                }

                match function.as_str() {
                    // TODO: This is rather unelegant, that only this function is handled here. Maybe we want to handle all function calls to special functions here, before converting to stack-based representation?
                    "is_some" => {
                        let hir::Type::Option(inner) = arguments[0].ty.clone() else {
                            panic!("can only call 'is_some' on optionals!")
                        };
                        let ty = inner.convert(self.lookup).unwrap();
                        Instruction::Drop { ty }
                    }
                    "unwrap_unsafe" => {
                        let ty = arguments[0].ty.clone();
                        let hir::Type::Option(_) = ty.clone() else {
                            panic!("can only call 'unwrap_unsafe' on optionals!")
                        };
                        let parent = self.locals.len() as u32;
                        self.locals.push(ty.convert(self.lookup).unwrap());
                        self.instructions.push(Instruction::LocalSet(parent));
                        Instruction::MemberGet { parent, member: 1 }
                    }
                    name => Instruction::Call(name.to_owned()),
                }
            }
            hir::ExpressionKind::Member {
                of: parent,
                name: field,
            } => {
                let record = match parent
                    .ty
                    .clone()
                    .convert(self.lookup)
                    .expect("Record should have a valid type")
                {
                    TypeRef::Primitive(_) => panic!("Cannot access member of a primitive"),
                    ty @ TypeRef::Type(index) => {
                        self.locals.push(ty);
                        self.lookup.types[index as usize].clone()
                    }
                };
                let Type::Record { fields } = record else {
                    panic!("ERROR: can only access fields of records");
                };
                let index = self.locals.len().sub(1) as u32;

                // TODO: climb parents to get to the top most struct, only push parent after arriving at the origin of the struct -- ideally optimize when the struct is obtained via a variable access
                self.push(*parent);
                self.instructions.push(Instruction::LocalSet(index));

                let pos = fields
                    .iter()
                    .position(|(name, _)| *name == field)
                    .expect("Field should exist");

                Instruction::MemberGet {
                    parent: index,
                    // TODO: climb parents to get to the top most struct
                    member: pos,
                }
            }
            hir::ExpressionKind::IndexAccess { of: target, index } => {
                let ty = expression
                    .ty
                    .clone()
                    .convert(self.lookup)
                    .expect("List elements must have a type");

                self.push(*target);
                self.push(*index);

                Instruction::IndexAccess { ty }
            }
            hir::ExpressionKind::Return(inner) => {
                self.push(*inner);

                Instruction::Return
            }
            hir::ExpressionKind::Break(inner) => {
                self.push(*inner);

                Instruction::Break
            }
            hir::ExpressionKind::Continue => Instruction::Continue,
            hir::ExpressionKind::Throw(inner) => {
                self.push(*inner);

                todo!("What to do with throw?")
            }
            hir::ExpressionKind::If {
                condition,
                then,
                else_,
            } => {
                self.push(*condition);
                Instruction::If {
                    then: then.convert(self.lookup, self.locals),
                    else_: else_.convert(self.lookup, self.locals),
                }
            }
            hir::ExpressionKind::Match { arms } => todo!(),
            hir::ExpressionKind::Loop(block) => {
                Instruction::Loop(block.convert(self.lookup, self.locals))
            }
            hir::ExpressionKind::Unreachable => Instruction::Unreachable,

            hir::ExpressionKind::Parameter(_) => todo!(),
            hir::ExpressionKind::Reference(id) => todo!(),
            hir::ExpressionKind::VarSet { var, expression } => {
                self.push(*expression);
                Instruction::LocalSet(var.0 as u32)
            }
            hir::ExpressionKind::VarGet { var } => Instruction::LocalGet(var.0 as u32),
            hir::ExpressionKind::None => Instruction::Noop,
            hir::ExpressionKind::Zero => Instruction::Zero {
                ty: expression.ty.clone().convert(self.lookup).unwrap(),
            },
            hir::ExpressionKind::Tuple(_) => todo!(),
            hir::ExpressionKind::TupleAccess { of, index } => todo!(),
            hir::ExpressionKind::Type(_) => todo!("This should probably just be a no-op"),
            hir::ExpressionKind::Multiple(exprs) => {
                for e in exprs {
                    self.push(e);
                }
                return;
            }
        };

        // This is to make WASM understand that the end of the function can never be reached
        match (expression.ty, instruction) {
            (_, instruction @ (Instruction::Unreachable | Instruction::Return)) => {
                self.instructions.push(instruction)
            }
            (hir::Type::Never, instruction) => {
                self.instructions.push(instruction);
                self.instructions.push(Instruction::Unreachable);
            }
            (_, instruction) => self.instructions.push(instruction),
        }
    }
}

impl Convert<'_> for hir::Type {
    type Output = Option<TypeRef>;

    fn convert(self, lookup: &mut Lookup) -> Self::Output {
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
            hir::Type::Resource { methods } => todo!(),
            hir::Type::Enum { cases } => {
                let ty = lookup.insert(Type::Enum(cases));

                Some(TypeRef::Type(ty))
            }
            hir::Type::Variant { cases } => todo!(),
            hir::Type::Tuple(_) => todo!(),
            hir::Type::None => None,
            hir::Type::Never => todo!("Disallow 'Never' for exported functions"),
            hir::Type::Blank => todo!("'Blank' not allowed here"),
            hir::Type::Bool => Some(TypeRef::Primitive(PrimitiveType::Bool)),
            hir::Type::Char => Some(TypeRef::Primitive(PrimitiveType::Char)),
            hir::Type::Int { signed, bits } => {
                let ty = match (signed, bits) {
                    (false, 8) => PrimitiveType::U8,
                    (true, 8) => PrimitiveType::S8,
                    (false, 16) => PrimitiveType::U16,
                    (true, 16) => PrimitiveType::S16,
                    (false, 32) => PrimitiveType::U32,
                    (true, 32) => PrimitiveType::S32,
                    (false, 64) => PrimitiveType::U64,
                    (true, 64) => PrimitiveType::S64,
                    _ => unreachable!("invalid integer type"),
                };

                Some(TypeRef::Primitive(ty))
            }
            hir::Type::Float { bits } => {
                let ty = match bits {
                    32 => PrimitiveType::F32,
                    64 => PrimitiveType::F64,
                    _ => unreachable!("invalid float type"),
                };

                Some(TypeRef::Primitive(ty))
            }
            hir::Type::String => Some(TypeRef::Primitive(PrimitiveType::String)),
            hir::Type::Meta(_) => todo!(),
        }
    }
}
