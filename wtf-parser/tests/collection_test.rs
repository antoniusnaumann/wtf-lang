use wtf_parser::lexer::Lexer;
use wtf_parser::parser::Parser;

#[test]
fn test_parse_array_type() {
    let input = r#"
    func test_array() {
        let numbers: [s32] = [1, 2, 3]
    }
    "#;

    let lexer = Lexer::new(input);
    let mut parser = Parser::with_lexer(lexer);
    let module = parser.parse_module().unwrap();

    let expected = r#"(module
  (func "test_array"
    (block
      (let "numbers"
        (list "s32")
        (list
          (int 1)
          (int 2)
          (int 3))))))"#;

    assert_eq!(module.to_string(), expected);
}

#[test]
fn test_parse_empty_array() {
    let input = r#"
    func test_empty_array() {
        let empty: [s32] = []
    }
    "#;

    let lexer = Lexer::new(input);
    let mut parser = Parser::with_lexer(lexer);
    let module = parser.parse_module().unwrap();

    let expected = r#"(module
  (func "test_empty_array"
    (block
      (let "empty"
        (list "s32")
        (list)))))"#;

    assert_eq!(module.to_string(), expected);
}