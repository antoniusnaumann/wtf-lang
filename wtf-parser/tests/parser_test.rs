use wtf_parser::lexer::Lexer;
use wtf_parser::parser::Parser;

#[test]
fn test_parse_function_declaration() {
    let input = r#"
        func add(a: s32, b: s32) -> s32 {
            return a + b
        }
        "#;

    let lexer = Lexer::new(input);
    let mut parser = Parser::with_lexer(lexer);
    let module = parser.parse_module().unwrap();

    let expected = r#"(module
  (func "add"
    (param "a" "s32")
    (param "b" "s32") "s32"
    (block
      (return
        (binary
          (ident a)
          (op +)
          (ident b))))))"#;
    
    assert_eq!(module.to_string(), expected);
}

#[test]
fn test_parse_variable_declaration() {
    let input = r#"
        func main() {
            let x = 42
            var y: s32 = x + 1
        }
        "#;

    let lexer = Lexer::new(input);
    let mut parser = Parser::with_lexer(lexer);
    let module = parser.parse_module().unwrap();

    let expected = r#"(module
  (func "main"
    (block
      (let "x"
        (int 42))
      (var "y" "s32"
        (binary
          (ident x)
          (op +)
          (int 1))))))"#;
    
    assert_eq!(module.to_string(), expected);
}

#[test]
fn test_parse_package_declaration() {
    let input = r#"
        package pkg:all_features
        "#;

    let lexer = Lexer::new(input);
    let mut parser = Parser::with_lexer(lexer);
    let module = parser.parse_module().unwrap();

    let expected = r#"(module
  (package
    (path "pkg" "all_features")))"#;
    
    assert_eq!(module.to_string(), expected);
}

#[test]
fn test_parse_use_declaration() {
    let input = r#"
        use external:io/printer.{print}
        "#;

    let lexer = Lexer::new(input);
    let mut parser = Parser::with_lexer(lexer);
    let module = parser.parse_module().unwrap();

    let expected = r#"(module
  (use
    (path "external" "io") "printer"
    (print)))"#;
    
    assert_eq!(module.to_string(), expected);
}

#[test]
fn test_parse_assignment() {
    let input = r#"
        func increment() {
            self.value = self.value + 1
        }
        "#;

    let lexer = Lexer::new(input);
    let mut parser = Parser::with_lexer(lexer);
    let module = parser.parse_module().unwrap();

    let expected = r#"(module
  (func "increment"
    (block
      (assign
        (access
          (ident self) "value")
        (binary
          (access
            (ident self) "value")
          (op +)
          (int 1))))))"#;
    
    assert_eq!(module.to_string(), expected);
}

#[test]
fn test_parse_list_literal() {
    let input = r#"
        func test_list() {
            let numbers = [1, 2, 3, 4]
        }
        "#;

    let lexer = Lexer::new(input);
    let mut parser = Parser::with_lexer(lexer);
    let module = parser.parse_module().unwrap();

    let expected = r#"(module
  (func "test_list"
    (block
      (let "numbers"
        (list
          (int 1)
          (int 2)
          (int 3)
          (int 4))))))"#;
    
    assert_eq!(module.to_string(), expected);
}
