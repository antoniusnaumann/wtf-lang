use std::{env, fmt::Display, fs, ops::Deref};

use wtf_parser::parser::Parser;

enum Mode {
    Build,
    Run,
    Test,
}

fn main() -> Result<(), i64> {
    let args: Vec<_> = env::args().collect();
    let mut args: Vec<_> = args.iter().map(|arg| arg.as_str()).collect();

    let verbose = {
        if let Some(pos) = args
            .iter()
            .position(|&arg| arg == "--verbose" || arg == "-v")
        {
            args.remove(pos);
            true
        } else {
            false
        }
    };
    let (mode, file_path) = match args.as_slice() {
        [.., "run", file] => (Mode::Run, file),
        [.., "test", file] => (Mode::Test, file),
        [file] => (Mode::Build, file),
        other => panic!("Illegal arguments: {:#?}", other),
    };

    let input = fs::read_to_string(file_path).unwrap();
    let compiler = Compiler { verbose, mode };

    compiler.execute(input)
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
    mode: Mode,
}

impl Compiler {
    fn execute(&self, input: String) -> Result<(), i64> {
        if self.verbose {
            println!("WTF!");
        }

        let mut parser = Parser::new(&input);
        if self.verbose {
            println!();
            println!("===== AST =====");
        }
        let mut ast = parser.parse_module().expect("No AST.");

        // Remove tests if we are not running the test command
        if !matches!(self.mode, Mode::Test) {
            ast.declarations.retain(|decl| match decl {
                wtf_ast::Declaration::Test(_) => false,
                _ => true,
            })
        }

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

        match self.mode {
            Mode::Build => Ok(()),
            Mode::Run => {
                if self.verbose {
                    println!();
                    println!("===== OUT =====");

                    println!();
                }
                let result = self.run(&wasm).unwrap();

                if self.verbose {
                    println!("'main': {}", result);
                }

                match result {
                    MainOutput::ExitCode(0) => Ok(()),
                    MainOutput::Message(msg) => {
                        println!("{}", msg);

                        Ok(())
                    }
                    MainOutput::ExitCode(status) => Err(status),
                }
            }
            Mode::Test => todo!(),
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
