use wtf_parser::lexer::Lexer;
use wtf_parser::parser::Parser;

#[test]
fn test_recover_function_declaration() {
    let input = r#"
        func add(a: s32, b: s32) -> s32 {
            return a + b =
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
    assert_eq!(parser.errors().len(), 1);
}

#[test]
fn test_recover_if_body() {
    let input = r#"
        func something() {
            if true {
                = this is illegal
                more stuff
                // and a comment
            }
            // This should work again        
            return a + b
        }
        "#;

    let lexer = Lexer::new(input);
    let mut parser = Parser::with_lexer(lexer);
    let module = parser.parse_module().unwrap();

    let expected = r#"(module
  (func "something"
    (block
      (if
        (bool true)
        (block))
      (return
        (binary
          (ident a)
          (op +)
          (ident b))))))"#;

    assert_eq!(module.to_string(), expected);
    assert_eq!(parser.errors().len(), 1);
}

#[test]
fn test_recover_nested_if_body() {
    let input = r#"
        func something() {
            if true {
                = this is illegal
                if something {
                    if a {
                        
                    }
                } else {
                    let c = a + b
                }
                more stuff
                // and a comment
            }
            // This should work again        
            return a + b
        }
        "#;

    let lexer = Lexer::new(input);
    let mut parser = Parser::with_lexer(lexer);
    let module = parser.parse_module().unwrap();

    let expected = r#"(module
  (func "something"
    (block
      (if
        (bool true)
        (block))
      (return
        (binary
          (ident a)
          (op +)
          (ident b))))))"#;

    assert_eq!(module.to_string(), expected);
    assert_eq!(parser.errors().len(), 1);
}