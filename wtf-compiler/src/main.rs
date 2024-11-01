use std::{env, fs};

use wtf_parser::parser::Parser;

fn main() {
    println!("WTF!");

    let args: Vec<String> = env::args().collect();
    let file_path = &args[1];

    let input = fs::read_to_string(file_path).unwrap();

    let mut parser = Parser::new(&input);
    println!();
    println!("===== AST =====");
    let ast = parser.parse_module().expect("No AST.");
    println!("{ast}");

    println!();
    println!("===== HIR =====");
    let hir = wtf_hir::compile(ast);
    println!("{hir}");

    println!();
    println!("===== WAT =====");
    let wasm = wtf_encode::Encoder::new().encode(hir).finish();
    println!("{:?}", wasmparser::validate(&wasm).err());

    fs::write("output.wasm", wasm).unwrap();
}
