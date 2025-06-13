use wtf_parser::lexer::Lexer;
use wtf_parser::parser::Parser;

#[test]
fn test_parse_record_literal() {
    let input = r#"
        func test_function() {
            let point = {
                x: 1.0,
                y: 2.0
            }
        }
        "#;

    let lexer = Lexer::new(input);
    let mut parser = Parser::with_lexer(lexer);
    let module = parser.parse_module().unwrap();

    let expected = r#"(module
  (func "test_function"
    (block
      (let "point"
        (record
          (fieldinit "x"
            (float 1))
          (fieldinit "y"
            (float 2)))))))"#;
    
    assert_eq!(module.to_string(), expected);
}

#[test]
fn test_parse_nested_record_literal() {
    let input = r#"
        func test_function() {
            let person = {
                name: "alice",
                address: {
                    street: "main st",
                    city: "wonderland"
                }
            }
        }
        "#;

    let lexer = Lexer::new(input);
    let mut parser = Parser::with_lexer(lexer);
    let module = parser.parse_module().unwrap();

    let expected = r#"(module
  (func "test_function"
    (block
      (let "person"
        (record
          (fieldinit "name"
            (string "alice"))
          (fieldinit "address"
            (record
              (fieldinit "street"
                (string "main st"))
              (fieldinit "city"
                (string "wonderland")))))))))"#;
    
    assert_eq!(module.to_string(), expected);
}