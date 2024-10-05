use std::{fs, io::Write};

use wtf_wasm::{ComponentBuilder, Function, Instance, Instruction, PrimitiveType, Type};

// This main serves as a usage example
// TODO: Move this to be a test instead
fn main() {
    let mut comp = ComponentBuilder::new();

    let functions = vec![Function {
        params: vec![],
        result: Some(Type::Primitive(PrimitiveType::S32)),
        name: "run".to_owned(),
        instructions: vec![Instruction::I32Const(21), Instruction::End],
        export: true,
    }];
    comp.encode_instance(Instance {
        name: "wasi:cli/run@0.2.0".to_owned(),
        types: vec![],
        functions,
    });

    comp.encode_instance(Instance {
        name: "antoniusnaumann:example/math".to_owned(),
        types: vec![],
        functions: vec![Function {
            params: vec![
                ("a".to_owned(), Type::Primitive(PrimitiveType::S32)),
                ("b".to_owned(), Type::Primitive(PrimitiveType::S32)),
            ],
            result: Some(Type::Primitive(PrimitiveType::S32)),
            name: "add".to_owned(),
            instructions: vec![
                Instruction::LocalGet(0),
                Instruction::LocalGet(1),
                Instruction::I32Add,
                Instruction::End,
            ],
            export: true,
        }],
    });

    let wasm_bytes = comp.finish();
    // println!("{:#?}", wasm_bytes);

    // TODO: To use this, activate the wasm_encode wasmparser feature for exec builds
    // println!("{:?}", wasmparser::validate(&wasm_bytes).err());

    // use std::time::{self, UNIX_EPOCH};
    // let timestamp = time::SystemTime::now()
    //     .duration_since(UNIX_EPOCH)
    //     .unwrap()
    //     .as_secs();
    let _ = fs::remove_file("output.wasm");
    let mut file = fs::OpenOptions::new()
        .create_new(true)
        .write(true)
        .open("output.wasm")
        .unwrap();

    file.write_all(&wasm_bytes).unwrap();
}
