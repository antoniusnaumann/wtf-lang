use std::{env, fmt::Display, fs, ops::Deref};

use wtf_parser::parser::Parser;

fn main() {
    println!("WTF!");

    let mut args: Vec<String> = env::args().collect();
    let verbose = {
        if let Some(pos) = args
            .iter()
            .position(|arg| arg == "--verbose" || arg == "-v")
        {
            args.remove(pos);
            true
        } else {
            false
        }
    };
    let is_run = args.len() > 2 && &args[1] == "run";
    let file_path = if is_run { &args[2] } else { &args[1] };

    let input = fs::read_to_string(file_path).unwrap();
    let compiler = Compiler {
        verbose,
        run: is_run,
    };

    compiler.compile(input);
}

#[derive(Debug)]
enum MainOutput {
    ExitCode(i64),
    Message(String),
}

impl Display for MainOutput {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            MainOutput::ExitCode(int) => write!(f, "{int}"),
            MainOutput::Message(str) => write!(f, "{str}"),
        }
    }
}

struct Compiler {
    verbose: bool,
    run: bool,
}

impl Compiler {
    fn compile(&self, input: String) {
        let mut parser = Parser::new(&input);
        if self.verbose {
            println!();
            println!("===== AST =====");
        }
        let ast = parser.parse_module().expect("No AST.");

        if self.verbose {
            println!("{ast}");

            println!();
            println!("===== HIR =====");
        }
        let hir = wtf_hir::compile(ast);
        if self.verbose {
            println!("{hir}");

            println!();
            println!("===== WAT =====");
        }
        let wasm = wtf_encode::Encoder::new().encode(hir).finish();
        if self.verbose {
            println!("{:?}", wasmparser::validate(&wasm).err());
        }

        fs::write("output.wasm", &wasm).unwrap();

        if self.run {
            if self.verbose {
                println!();
                println!("===== OUT =====");

                println!();
            }
            println!("Exit Code: {}", self.run(&wasm).unwrap());
        }
    }

    fn run(&self, wasm: &[u8]) -> wasmtime::Result<MainOutput> {
        use wasmtime::{
            component::{Component, Linker, Type, Val},
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

            return Ok(MainOutput::ExitCode(0));
        }

        let mut result = match func.results(&store).deref() {
            [Type::String] => [Val::String(String::new())],
            [Type::S64] => [Val::S64(0)],
            _ => {
                panic!("main function had unexpected result type!");
            }
        };

        func.call(&mut store, &[], &mut result)?;

        match result {
            [Val::String(str)] => Ok(MainOutput::Message(str)),
            [Val::S64(int)] => Ok(MainOutput::ExitCode(int)),
            _ => panic!("main function returned unexpected result!"),
        }
    }
}
