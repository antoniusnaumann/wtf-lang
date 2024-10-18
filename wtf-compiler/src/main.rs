use wtf_parser::parser::Parser;

fn main() {
    println!("WTF!");

    let input = r#"
        package wtf:example@0.0.1
    
        func check_value(x: s32) {
            let y = 17
            if y > 10 {
                println("Greater than 10")
            } else {
                println("10 or less")
            }
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
}
