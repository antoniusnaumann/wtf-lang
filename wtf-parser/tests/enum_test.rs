use wtf_parser::lexer::Lexer;
use wtf_parser::parser::Parser;

#[test]
fn test_parse_enum_declaration() {
    let input = r#"
    enum color {
        red
        green
        blue
    }
    "#;

    let lexer = Lexer::new(input);
    let mut parser = Parser::with_lexer(lexer);
    let module = parser.parse_module().unwrap();

    let expected = r#"(module
  (enum "color"
    (case "red")
    (case "green")
    (case "blue")))"#;

    assert_eq!(module.to_string(), expected);
}

#[test]
fn test_parse_variant_declaration_without_associated_types() {
    let input = r#"
    variant shape {
        circle
        square
        triangle
    }
    "#;

    let lexer = Lexer::new(input);
    let mut parser = Parser::with_lexer(lexer);
    let module = parser.parse_module().unwrap();

    let expected = r#"(module
  (variant
    (case "circle")
    (case "square")
    (case "triangle")))"#;

    assert_eq!(module.to_string(), expected);
}

#[test]
fn test_parse_variant_declaration_with_associated_types() {
    let input = r#"
    variant result {
        ok(value: s32)
        err(error: string)
    }
    "#;

    let lexer = Lexer::new(input);
    let mut parser = Parser::with_lexer(lexer);
    let module = parser.parse_module().unwrap();

    let expected = r#"(module
  (variant
    (case "ok"
      (field "value" "s32"))
    (case "err"
      (field "error" "string"))))"#;

    assert_eq!(module.to_string(), expected);
}

#[test]
fn test_parse_variant_declaration_with_mixed_cases() {
    let input = r#"
    variant response {
        success
        failure(code: s32, message: string)
    }
    "#;

    let lexer = Lexer::new(input);
    let mut parser = Parser::with_lexer(lexer);
    let module = parser.parse_module().unwrap();

    let expected = r#"(module
  (variant
    (case "success")
    (case "failure"
      (field "code" "s32")
      (field "message" "string"))))"#;

    assert_eq!(module.to_string(), expected);
}

#[test]
fn test_parse_empty_enum_declaration() {
    let input = r#"
    enum empty_enum {}
    "#;

    let lexer = Lexer::new(input);
    let mut parser = Parser::with_lexer(lexer);
    let module = parser.parse_module().unwrap();

    let expected = r#"(module
  (enum "empty_enum"))"#;

    assert_eq!(module.to_string(), expected);
}