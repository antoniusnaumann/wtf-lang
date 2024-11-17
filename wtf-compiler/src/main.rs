use std::{env, fs};

use wtf_parser::parser::Parser;

fn main() {
    println!("WTF!");

    let args: Vec<String> = env::args().collect();
    let is_run = args.len() > 2 && &args[1] == "run";
    let file_path = if is_run { &args[2] } else { &args[1] };

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

    fs::write("output.wasm", &wasm).unwrap();

    if is_run {
        println!();
        println!("===== OUT =====");

        println!();
        println!("Exit Code: {}", run(&wasm).unwrap());
    }
}

fn run(wasm: &[u8]) -> wasmtime::Result<i64> {
    use wasmtime::{
        component::{Component, Linker, Val},
        Config, Engine, Store,
    };

    let mut config = Config::new();
    config.wasm_component_model(true);

    let engine = Engine::new(&config)?;
    let mut store = Store::new(&engine, {});
    let mut linker = Linker::new(&engine);
    linker
        .root()
        .func_wrap("println", |_store, params: (String,)| {
            println!("{}", params.0);
            Ok(())
        })?;

    let component = Component::from_binary(&engine, wasm)?;

    let instance = linker.instantiate(&mut store, &component)?;

    let func = instance
        .get_func(&mut store, "main")
        .expect("main function did not exist");

    // This allows main functions without return value
    if func.results(&store).len() == 0 {
        let mut result = [];
        func.call(&mut store, &[], &mut result)?;

        return Ok(0);
    }

    let mut result = [wasmtime::component::Val::S64(0)];
    func.call(&mut store, &[], &mut result)?;

    let Val::S64(result) = result[0] else {
        panic!("main function had unexpected result type!")
    };

    Ok(result)
}
