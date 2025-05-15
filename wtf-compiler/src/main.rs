use std::{env, fmt::Display, fs, num::NonZeroUsize, ops::Deref};

use wtf_hir::Test;
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
        [.., "build", file] => (Mode::Build, file),
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

#[derive(Debug)]
enum TestError {
    WasmRuntime(wasmtime::Error),
    TestFail(NonZeroUsize),
}

impl From<wasmtime::Error> for TestError {
    fn from(value: wasmtime::Error) -> Self {
        Self::WasmRuntime(value)
    }
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
            if self.verbose {
                println!("Removing tests...");
                println!();
            }
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
        let tests = hir.tests.clone();
        if self.verbose {
            println!("{hir}");

            println!();
            println!("===== MIR =====");
        }

        // let mir = wtf_mir::compile(hir);
        // if self.verbose {
        //     println!("{mir}");

        //     println!();
        //     println!("===== WAT =====");
        // }

        Ok(())
        // let wasm = wtf_encode::Encoder::new().encode(hir).finish();
        // if self.verbose {
        //     println!("{:?}", wasmparser::validate(&wasm).err());
        // }

        // fs::write("output.wasm", &wasm).unwrap();

        // match self.mode {
        //     Mode::Build => Ok(()),
        //     Mode::Run => {
        //         if self.verbose {
        //             println!();
        //             println!("===== OUT =====");
        //         }
        //         let result = self.run(&wasm).unwrap();

        //         if self.verbose {
        //             println!("'main': {}", result);
        //         }

        //         match result {
        //             MainOutput::ExitCode(0) => Ok(()),
        //             MainOutput::Message(msg) => {
        //                 println!("{}", msg);

        //                 Ok(())
        //             }
        //             MainOutput::ExitCode(status) => Err(status),
        //         }
        //     }
        //     Mode::Test => {
        //         if self.verbose {
        //             println!();
        //             println!("===== OUT =====");
        //         }

        //         self.run_tests(&wasm, &tests).map_err(|_| 22)
        //     } // TODO: find out which error code to return here
        // }
    }

    fn run_tests(&self, wasm: &[u8], tests: &[Test]) -> Result<(), TestError> {
        use wasmtime::{
            component::{Component, Linker},
            Config, Engine, Store,
        };

        let mut config = Config::new();
        config.wasm_component_model(true);

        let engine = Engine::new(&config)?;
        let mut store = Store::new(&engine, {});
        let linker = Linker::new(&engine);
        let component = Component::from_binary(&engine, wasm)?;

        let mut fail = 0;
        let mut pass = 0;

        for test in tests {
            let instance = linker.instantiate(&mut store, &component)?;
            let func = instance
                .get_func(&mut store, &test.id)
                .expect("test function did not exist");
            let name = test
                .name
                .as_ref()
                .map(|n| format!("\"{n}\""))
                .unwrap_or_else(|| "<test>".to_owned());

            match func.call(&mut store, &[], &mut []) {
                Ok(_) => {
                    pass += 1;
                    println!("PASS -- {name}")
                }
                Err(e) => {
                    fail += 1;
                    println!("FAIL -- {name}");
                    if self.verbose {
                        println!("{e}");
                    }
                }
            }
        }

        println!();
        println!("ran {} tests:", tests.len());
        println!("{pass} passed, {fail} failed");

        if let Ok(fail) = NonZeroUsize::try_from(fail) {
            println!();
            Err(TestError::TestFail(fail))
        } else {
            Ok(())
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
