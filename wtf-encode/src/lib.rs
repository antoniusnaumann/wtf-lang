use wtf_hir as hir;
use wtf_wasm::{ComponentBuilder, Function, Instance, Instruction, Type};

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
        todo!()
    }
}
