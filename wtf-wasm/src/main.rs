use std::{fs, io::Write};

use wtf_wasm::{
    ComponentBuilder, Function, Instance, InstructionVec, PrimitiveType, TypeRef, WasmInstruction,
};

// This main serves as a usage example
// TODO: Move this to be a test instead
fn main() {
    let mut comp = ComponentBuilder::new();

    let functions = vec![Function {
        params: vec![],
        result: Some(TypeRef::Primitive(PrimitiveType::S32)),
        name: "run".to_owned(),
        instructions: vec![WasmInstruction::I32Const(21), WasmInstruction::End].into_instructions(),
        export: true,
        locals: vec![],
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
                ("a".to_owned(), TypeRef::Primitive(PrimitiveType::S32)),
                ("b".to_owned(), TypeRef::Primitive(PrimitiveType::S32)),
            ],
            result: Some(TypeRef::Primitive(PrimitiveType::S32)),
            name: "add".to_owned(),
            instructions: vec![
                WasmInstruction::LocalGet(0),
                WasmInstruction::LocalGet(1),
                WasmInstruction::I32Add,
                WasmInstruction::End,
            ]
            .into_instructions(),
            export: true,
            locals: vec![],
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
