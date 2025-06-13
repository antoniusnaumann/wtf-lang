use wtf_parser::lexer::Lexer;
use wtf_parser::parser::Parser;

#[test]
fn test_export_function() {
    let input = r#"
        export func greet(name: string) -> string {
            return "Hello, " + name
        }
        "#;

    let lexer = Lexer::new(input);
    let mut parser = Parser::with_lexer(lexer);
    let module = parser.parse_module().unwrap();

    let expected = r#"(module
  (export
    (func "greet"
      (param "name" "string") "string"
      (block
        (return
          (binary
            (string "Hello, ")
            (op +)
            (ident name)))))))"#;

    assert_eq!(module.to_string(), expected);
}

#[test]
fn test_export_record() {
    let input = r#"
        export record person {
            name: string,
            age: u32,
        }
        "#;

    let lexer = Lexer::new(input);
    let mut parser = Parser::with_lexer(lexer);
    let module = parser.parse_module().unwrap();

    let expected = r#"(module
  (export
    (record "person"
      (field "name" "string")
      (field "age" "u32"))))"#;

    assert_eq!(module.to_string(), expected);
}

#[test]
fn test_export_resource() {
    let input = r#"
        export resource file {
            path: string

            constructor(path: string) {
                self.path = path
            }

            func read() -> string {
                // Read file logic here
                return "file content"
            }
        }
        "#;

    let lexer = Lexer::new(input);
    let mut parser = Parser::with_lexer(lexer);
    let module = parser.parse_module().unwrap();

    let expected = r#"(module
  (export
    (resource "file"
      (field "path" "string")
      (func "read" "string"
        (block
          (return
            (string "file content")))))))"#;

    assert_eq!(module.to_string(), expected);
}

#[test]
fn test_export_enum() {
    let input = r#"
        export enum color {
            red,
            green,
            blue,
        }
        "#;

    let lexer = Lexer::new(input);
    let mut parser = Parser::with_lexer(lexer);
    let module = parser.parse_module().unwrap();

    let expected = r#"(module
  (export
    (enum "color"
      (case "red")
      (case "green")
      (case "blue"))))"#;

    assert_eq!(module.to_string(), expected);
}

#[test]
fn test_export_variant() {
    let input = r#"
        export variant shape {
            circle(radius: f64),
            rectangle(width: f64, height: f64),
        }
        "#;

    let lexer = Lexer::new(input);
    let mut parser = Parser::with_lexer(lexer);
    let module = parser.parse_module().unwrap();

    let expected = r#"(module
  (export
    (variant
      (case "circle"
        (field "radius" "f64"))
      (case "rectangle"
        (field "width" "f64")
        (field "height" "f64")))))"#;

    assert_eq!(module.to_string(), expected);
}

#[test]
fn test_export_with_package_and_use() {
    let input = r#"
        package my_module:my_package@1.0.0

        use some_dependency:some_package/some_mod.{some_type, some_other_type}

        export func do_something() {
            // Function body
        }
        "#;

    let lexer = Lexer::new(input);
    let mut parser = Parser::with_lexer(lexer);
    let module = parser.parse_module().unwrap();

    let expected = r#"(module
  (package
    (path "my_module" "my_package")
    (version 1 0 0))
  (use
    (path "some_dependency" "some_package") "some_mod"
    (some_type some_other_type))
  (export
    (func "do_something"
      (block))))"#;

    assert_eq!(module.to_string(), expected);
}