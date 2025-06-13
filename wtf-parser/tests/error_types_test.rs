use wtf_parser::lexer::Lexer;
use wtf_parser::parser::Parser;

#[test]
fn test_parse_function_return_type_with_result() {
    let input = r#"
    func test_function() -> mytype!myerror {
        // Function body
    }
    "#;

    let lexer = Lexer::new(input);
    let mut parser = Parser::with_lexer(lexer);
    let module = parser.parse_module().unwrap();

    let expected = r#"(module
  (func "test_function"
    (result "mytype" "myerror")
    (block)))"#;

    assert_eq!(module.to_string(), expected);
}

#[test]
fn test_parse_record_field_with_result_type() {
    let input = r#"
    record myrecord {
        field: mytype!myerror
    }
    "#;

    let lexer = Lexer::new(input);
    let mut parser = Parser::with_lexer(lexer);
    let module = parser.parse_module().unwrap();

    let expected = r#"(module
  (record "myrecord"
    (field "field"
      (result "mytype" "myerror"))))"#;

    assert_eq!(module.to_string(), expected);
}

#[test]
fn test_parse_function_return_type_with_result_default_error() {
    let input = r#"
    func test_function() -> mytype! {
        // Function body
    }
    "#;

    let lexer = Lexer::new(input);
    let mut parser = Parser::with_lexer(lexer);
    let module = parser.parse_module().unwrap();

    let expected = r#"(module
  (func "test_function"
    (result "mytype" "error")
    (block)))"#;

    assert_eq!(module.to_string(), expected);
}

#[test]
fn test_parse_record_field_with_result_type_default_error() {
    let input = r#"
    record my_record {
        field1: mytype!
    }
    "#;

    let lexer = Lexer::new(input);
    let mut parser = Parser::with_lexer(lexer);
    let module = parser.parse_module().unwrap();

    let expected = r#"(module
  (record "my_record"
    (field "field1"
      (result "mytype" "error"))))"#;

    assert_eq!(module.to_string(), expected);
}

#[test]
fn test_parse_function_return_type_result_optional() {
    let input = r#"
    func test_function() -> mytype!myerror? {
        // function body
    }
    "#;

    let lexer = Lexer::new(input);
    let mut parser = Parser::with_lexer(lexer);
    let module = parser.parse_module().unwrap();

    let expected = r#"(module
  (func "test_function"
    (option
      (result "mytype" "myerror"))
    (block)))"#;

    assert_eq!(module.to_string(), expected);
}

#[test]
fn test_parse_record_field_with_result_optional_type() {
    let input = r#"
    record my_record {
        field1: mytype!myerror?
    }
    "#;

    let lexer = Lexer::new(input);
    let mut parser = Parser::with_lexer(lexer);
    let module = parser.parse_module().unwrap();

    let expected = r#"(module
  (record "my_record"
    (field "field1"
      (option
        (result "mytype" "myerror")))))"#;

    assert_eq!(module.to_string(), expected);
}