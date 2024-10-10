use wtf_parser::parser::Parser;

fn main() {
    println!("WTF!");

    let input = r#"
        func check_value(x: s32) {
            if x > 10 {
                println("Greater than 10")
            } else {
                println("10 or less")
            }
        }
        "#;

    let mut parser = Parser::new(input);
    let ast = parser.parse_module().expect("No AST.");
    let hir = wtf_hir::compile(ast);
    println!("HIR: {hir}");
}
