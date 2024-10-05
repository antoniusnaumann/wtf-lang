use wtf_hir as hir;
use wtf_wasm::{ComponentBuilder, Function, Instance, Instruction, PrimitiveType, Type};

#[derive(Debug, Default)]
pub struct Encoder<'a> {
    builder: ComponentBuilder<'a>,
}

impl<'a> Encoder<'a> {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn encode(mut self, module: hir::Module) -> Encoder<'a> {
        let instance = module.convert();
        self.builder.encode_instance(instance);

        self
    }

    pub fn finish(self) -> Vec<u8> {
        self.builder.finish()
    }
}

trait Convert<'a> {
    type Output;

    fn convert(self) -> Self::Output;
}

impl<'a> Convert<'a> for hir::Module {
    type Output = Instance<'a>;

    fn convert(self) -> Self::Output {
        let types = self.types.into_iter().map(Convert::convert).collect();
        let functions = self.functions.into_iter().map(Convert::convert).collect();

        Instance {
            name: "todo".to_owned(),
            functions,
            types,
        }
    }
}

impl Convert<'_> for (String, hir::Type) {
    type Output = Type;

    fn convert(self) -> Self::Output {
        todo!()
    }
}

impl<'a> Convert<'a> for (String, hir::Function) {
    type Output = Function<'a>;

    fn convert(self) -> Self::Output {
        let (name, func) = self;
        let params: Vec<_> = func
            .parameters
            .into_iter()
            .map(|(name, ty)| (name, ty.convert()))
            .collect();
        let result = func.return_type.map(|result| result.convert());
        // TODO: @Marcel, can you handle this?
        // let instructions: Vec<Instruction> = func
        //     .body
        //     .into_iter()
        //     .map(|st| todo!("Create instructions from statements"))
        //     .chain(iter::once(Instruction::End))
        //     .collect();
        let instructions = vec![Instruction::End];
        let func = Function {
            params,
            result,
            name,
            instructions,
            // TODO: only export functions with export keyword
            export: true,
        };

        func
    }
}

impl Convert<'_> for hir::Type {
    type Output = Type;

    fn convert(self) -> Self::Output {
        match self {
            hir::Type::List(_) => todo!(),
            hir::Type::Option(_) => todo!(),
            hir::Type::Result { ok, err } => todo!(),
            hir::Type::Record(_) => todo!(),
            hir::Type::Resource(_) => todo!(),
            hir::Type::Enum(_) => todo!(),
            hir::Type::Variant(_) => todo!(),
            hir::Type::Tuple(_) => todo!(),
            hir::Type::Builtin(ty) => Type::Primitive(ty.convert()),
        }
    }
}

impl Convert<'_> for hir::PrimitiveType {
    type Output = PrimitiveType;

    fn convert(self) -> Self::Output {
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
