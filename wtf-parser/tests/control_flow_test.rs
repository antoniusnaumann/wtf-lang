use wtf_parser::lexer::Lexer;
use wtf_parser::parser::Parser;

#[test]
fn test_parse_if_statement() {
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
    let mut parser = Parser::with_lexer(lexer);
    let module = parser.parse_module().unwrap();

    let expected = r#"(module
  (func "check_value"
    (param "x" "s32")
    (block
      (if
        (binary
          (ident x)
          (op >)
          (int 10))
        (block
          (call
            (ident println)
            (args
              (string "Greater than 10"))))
        (block
          (call
            (ident println)
            (args
              (string "10 or less"))))))))"#;
    
    assert_eq!(module.to_string(), expected);
}

#[test]
fn test_parse_return_statement() {
    let input = r#"
        func get_value() -> s32 {
            return 42
        }
        "#;

    let lexer = Lexer::new(input);
    let mut parser = Parser::with_lexer(lexer);
    let module = parser.parse_module().unwrap();

    let expected = r#"(module
  (func "get_value" "s32"
    (block
      (return
        (int 42)))))"#;
    
    assert_eq!(module.to_string(), expected);
}