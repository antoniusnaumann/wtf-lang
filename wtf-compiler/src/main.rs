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

    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);
    let ast = parser.parse_module()?;

    let hir = ast.
}
