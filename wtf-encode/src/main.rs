use std::{
    error::Error,
    fs::File,
    io::{Read, Write},
    iter,
};

use wtf_ast::TypeAnnotation;
use wtf_parser::parser::Parser;
use wtf_wasm::{ComponentBuilder, Function, Instance, Instruction, Type};

fn main() -> Result<(), Box<dyn Error>> {
    let mut file = File::open("main.wtf")?;
    let mut content = String::new();

    file.read_to_string(&mut content)?;

    let mut parser = Parser::new(&content);
    let module = parser.parse_module();
    let module = match module {
        Ok(m) => m,
        Err(e) => {
            eprintln!("Parser Error:\n\n{:#?}", e);
            return Err("Parsing failed".into());
        }
    };
    let mut builder = ComponentBuilder::new();

    let mut functions = Vec::new();
    for declaration in module.declarations {
        match declaration {
            wtf_ast::Declaration::Function(f) => {
                let params: Vec<_> = f
                    .parameters
                    .into_iter()
                    .map(|p| (p.name, convert_type(p.type_annotation)))
                    .collect();
                let result = f.return_type.map(convert_type);
                let instructions: Vec<Instruction> = f
                    .body
                    .statements
                    .into_iter()
                    .map(|st| todo!("Create instructions from statements"))
                    .chain(iter::once(Instruction::End))
                    .collect();
                let fun = Function {
                    params,
                    result,
                    name: f.name,
                    instructions,
                    // TODO: only export functions with export keyword
                    export: true,
                };
                functions.push(fun);
            }
            wtf_ast::Declaration::Record(_) => todo!(),
            wtf_ast::Declaration::Resource(_) => todo!(),
            wtf_ast::Declaration::Enum(_) => todo!(),
            wtf_ast::Declaration::Variant(_) => todo!(),
            wtf_ast::Declaration::Export(_) => todo!(),
            wtf_ast::Declaration::Package(_) => todo!(),
            wtf_ast::Declaration::Use(_) => todo!(),
        }
    }

    let instance = Instance {
        name: "antoniusnaumann:example/main".to_owned(),
        functions,
    };
    builder.encode_instance(instance);

    let wasm_bytes = builder.finish();
    println!("{:?}", wasmparser::validate(&wasm_bytes).err());

    let _ = std::fs::remove_file("output.wasm");
    let mut file = File::create("output.wasm")?;

    file.write_all(&wasm_bytes)?;

    Ok(())
}

fn convert_type(ty: TypeAnnotation) -> Type {
    match ty {
        wtf_ast::TypeAnnotation::Simple(s) => Type::Primitive(match s.as_str() {
            "s32" => wtf_wasm::PrimitiveType::S32,
            _ => todo!(),
        }),
        wtf_ast::TypeAnnotation::List(_) => todo!(),
        wtf_ast::TypeAnnotation::Option(_) => todo!(),
        wtf_ast::TypeAnnotation::Result { ok, err } => todo!(),
        wtf_ast::TypeAnnotation::Tuple(_) => todo!(),
    }
}
