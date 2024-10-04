use wtf_ast::*;
use wtf_parser::lexer::Lexer;
use wtf_parser::parser::{Parser, Result};

#[test]
fn test_parse_function_return_type_with_result() -> Result<()> {
    let input = r#"
    func test_function() -> mytype!myerror {
        // Function body
    }
    "#;

    let lexer = Lexer::new(input);
    let mut parser = Parser::with_lexer(lexer);
    let module = parser.parse_module()?;

    let expected_ast = Module {
        declarations: vec![Declaration::Function(FunctionDeclaration {
            name: "test_function".to_string(),
            parameters: vec![],
            return_type: Some(TypeAnnotation::Result {
                ok: Box::new(TypeAnnotation::Simple("mytype".to_string())),
                err: Box::new(TypeAnnotation::Simple("myerror".to_string())),
            }),
            body: Block { statements: vec![] },
        })],
    };

    assert_eq!(module, expected_ast);

    Ok(())
}

#[test]
fn test_parse_record_field_with_result_type() -> Result<()> {
    let input = r#"
    record myrecord {
        field: mytype!myerror
    }
    "#;

    let lexer = Lexer::new(input);
    let mut parser = Parser::with_lexer(lexer);
    let module = parser.parse_module()?;

    let expected_ast = Module {
        declarations: vec![Declaration::Record(RecordDeclaration {
            name: "myrecord".to_string(),
            fields: vec![Field {
                name: "field".to_string(),
                type_annotation: TypeAnnotation::Result {
                    ok: Box::new(TypeAnnotation::Simple("mytype".to_string())),
                    err: Box::new(TypeAnnotation::Simple("myerror".to_string())),
                },
            }],
        })],
    };

    assert_eq!(module, expected_ast);

    Ok(())
}

#[test]
fn test_parse_function_return_type_with_result_default_error() -> Result<()> {
    let input = r#"
    func test_function() -> mytype! {
        // Function body
    }
    "#;

    let lexer = Lexer::new(input);
    let mut parser = Parser::with_lexer(lexer);
    let module = parser.parse_module()?;

    let expected_ast = Module {
        declarations: vec![Declaration::Function(FunctionDeclaration {
            name: "test_function".to_string(),
            parameters: vec![],
            return_type: Some(TypeAnnotation::Result {
                ok: Box::new(TypeAnnotation::Simple("mytype".to_string())),
                err: Box::new(TypeAnnotation::Simple("error".to_string())),
            }),
            body: Block { statements: vec![] },
        })],
    };

    assert_eq!(module, expected_ast);

    Ok(())
}

#[test]
fn test_parse_record_field_with_result_type_default_error() -> Result<()> {
    let input = r#"
    record my_record {
        field1: mytype!
    }
    "#;

    let lexer = Lexer::new(input);
    let mut parser = Parser::with_lexer(lexer);
    let module = parser.parse_module()?;

    // Assuming default error type 'error'
    let expected_ast = Module {
        declarations: vec![Declaration::Record(RecordDeclaration {
            name: "my_record".to_string(),
            fields: vec![Field {
                name: "field1".to_string(),
                type_annotation: TypeAnnotation::Result {
                    ok: Box::new(TypeAnnotation::Simple("mytype".to_string())),
                    err: Box::new(TypeAnnotation::Simple("error".to_string())), // Default error type
                },
            }],
        })],
    };

    assert_eq!(module, expected_ast);

    Ok(())
}

#[test]
fn test_parse_function_return_type_result_optional() -> Result<()> {
    let input = r#"
    func test_function() -> mytype!myerror? {
        // function body
    }
    "#;

    let lexer = Lexer::new(input);
    let mut parser = Parser::with_lexer(lexer);
    let module = parser.parse_module()?;

    // 'mytype!myerror?' is equivalent to 'option<result<mytype, myerror>>'
    let expected_ast = Module {
        declarations: vec![Declaration::Function(FunctionDeclaration {
            name: "test_function".to_string(),
            parameters: vec![],
            return_type: Some(TypeAnnotation::Option(Box::new(TypeAnnotation::Result {
                ok: Box::new(TypeAnnotation::Simple("mytype".to_string())),
                err: Box::new(TypeAnnotation::Simple("myerror".to_string())),
            }))),
            body: Block { statements: vec![] }, // Empty function body
        })],
    };

    assert_eq!(module, expected_ast);

    Ok(())
}

#[test]
fn test_parse_record_field_with_result_optional_type() -> Result<()> {
    let input = r#"
    record my_record {
        field1: mytype!myerror?
    }
    "#;

    let lexer = Lexer::new(input);
    let mut parser = Parser::with_lexer(lexer);
    let module = parser.parse_module()?;

    // 'mytype!myerror?' is equivalent to 'option<result<mytype, myerror>>'
    let expected_ast = Module {
        declarations: vec![Declaration::Record(RecordDeclaration {
            name: "my_record".to_string(),
            fields: vec![Field {
                name: "field1".to_string(),
                type_annotation: TypeAnnotation::Option(Box::new(TypeAnnotation::Result {
                    ok: Box::new(TypeAnnotation::Simple("mytype".to_string())),
                    err: Box::new(TypeAnnotation::Simple("myerror".to_string())),
                })),
            }],
        })],
    };

    assert_eq!(module, expected_ast);

    Ok(())
}
