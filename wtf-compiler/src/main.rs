use std::fs;

use wtf_parser::parser::Parser;

fn main() {
    println!("WTF!");

    let input = r#"
        package wtf:example@0.0.1
    
        func double_cap(x: s64, cap: s64) -> s64 {
            let y = x * 2
            if y > cap {
               return cap
            } else {
               return y
            }
        }

        record point {
            x: s64 
            y: s64
        }

        func double_point(p: point) -> point {
            return p
        }
        "#;

    let mut parser = Parser::new(input);
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
