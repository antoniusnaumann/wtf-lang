use wtf_parser::lexer::Lexer;
use wtf_parser::parser::Parser;

#[test]
fn test_parse_optional_chaining() {
    let input = r#"
    func test_optional_chaining() {
        let value = optional_value?.to_string()
    }
    "#;

    let lexer = Lexer::new(input);
    let mut parser = Parser::with_lexer(lexer);
    let module = parser.parse_module().unwrap();

    let expected = r#"(module
  (func "test_optional_chaining"
    (block
      (let "value"
        (safecall "to_string"
          (receiver
            (ident optional_value))
          (args))))))"#;

    assert_eq!(module.to_string(), expected);
}

#[test]
fn test_parse_null_coalescing_operator() {
    let input = r#"
    func test_null_coalescing() {
        let value: s32 = maybe_number ? 5
    }
    "#;

    let lexer = Lexer::new(input);
    let mut parser = Parser::with_lexer(lexer);
    let module = parser.parse_module().unwrap();

    let expected = r#"(module
  (func "test_null_coalescing"
    (block
      (let "value" "s32"
        (binary
          (ident maybe_number)
          (op ?)
          (int 5))))))"#;

    assert_eq!(module.to_string(), expected);
}

#[test]
fn test_parse_optional_assignment_operator() {
    let input = r#"
    func test_optional_assignment() {
        value ?= 42
    }
    "#;

    let lexer = Lexer::new(input);
    let mut parser = Parser::with_lexer(lexer);
    let module = parser.parse_module().unwrap();

    let expected = r#"(module
  (func "test_optional_assignment"
    (block
      (assign
        (ident value)
        (binary
          (ident value)
          (op ?)
          (int 42))))))"#;

    assert_eq!(module.to_string(), expected);
}

#[test]
fn test_parse_optional_type_with_init() {
    let input = r#"
    func test_optional_declaration() {
        var value: option<s32> = none
    }
    "#;

    let lexer = Lexer::new(input);
    let mut parser = Parser::with_lexer(lexer);
    let module = parser.parse_module().unwrap();

    let expected = r#"(module
  (func "test_optional_declaration"
    (block
      (var "value"
        (option "s32")
        (none)))))"#;

    assert_eq!(module.to_string(), expected);
}

#[test]
fn test_parse_optional_type() {
    let input = r#"
    func test_optional_declaration() {
        var value: option<s32>
    }
    "#;

    let lexer = Lexer::new(input);
    let mut parser = Parser::with_lexer(lexer);
    let module = parser.parse_module().unwrap();

    let expected = r#"(module
  (func "test_optional_declaration"
    (block
      (var "value"
        (option "s32")))))"#;

    assert_eq!(module.to_string(), expected);
}

#[test]
fn test_parse_optional_type_with_init_shorthand() {
    let input = r#"
    func test_optional_declaration() {
        var value: s32? = none
    }
    "#;

    let lexer = Lexer::new(input);
    let mut parser = Parser::with_lexer(lexer);
    let module = parser.parse_module().unwrap();

    let expected = r#"(module
  (func "test_optional_declaration"
    (block
      (var "value"
        (option "s32")
        (none)))))"#;

    assert_eq!(module.to_string(), expected);
}

#[test]
fn test_parse_optional_type_shorthand() {
    let input = r#"
    func test_optional_declaration() {
        var value: s32?
    }
    "#;

    let lexer = Lexer::new(input);
    let mut parser = Parser::with_lexer(lexer);
    let module = parser.parse_module().unwrap();

    let expected = r#"(module
  (func "test_optional_declaration"
    (block
      (var "value"
        (option "s32")))))"#;

    assert_eq!(module.to_string(), expected);
}