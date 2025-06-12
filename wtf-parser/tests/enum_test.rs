use wtf_ast::*;
use wtf_parser::lexer::Lexer;
use wtf_parser::parser::{Parser, Result};

#[test]
fn test_parse_enum_declaration() -> Result<()> {
    let input = r#"
    enum color {
        red
        green
        blue
    }
    "#;

    let lexer = Lexer::new(input);
    let mut parser = Parser::with_lexer(lexer);
    let module = parser.parse_module()?;

    let expected_ast = Module {
        declarations: vec![Declaration::Enum(EnumDeclaration {
            name: "color".to_string(),
            cases: vec!["red".to_string(), "green".to_string(), "blue".to_string()],
        })],
        package: None,
        uses: vec![],
    };

    assert_eq!(module, expected_ast);

    Ok(())
}

#[test]
fn test_parse_variant_declaration_without_associated_types() -> Result<()> {
    let input = r#"
    variant shape {
        circle
        square
        triangle
    }
    "#;

    let lexer = Lexer::new(input);
    let mut parser = Parser::with_lexer(lexer);
    let module = parser.parse_module()?;

    let expected_ast = Module {
        declarations: vec![Declaration::Variant(VariantDeclaration {
            name: "shape".to_string(),
            cases: vec![
                VariantCase {
                    name: "circle".to_string(),
                    associated_types: vec![],
                },
                VariantCase {
                    name: "square".to_string(),
                    associated_types: vec![],
                },
                VariantCase {
                    name: "triangle".to_string(),
                    associated_types: vec![],
                },
            ],
        })],
        package: None,
        uses: vec![],
    };

    assert_eq!(module, expected_ast);

    Ok(())
}

#[test]
fn test_parse_variant_declaration_with_associated_types() -> Result<()> {
    let input = r#"
    variant result {
        ok(value: s32)
        err(error: string)
    }
    "#;

    let lexer = Lexer::new(input);
    let mut parser = Parser::with_lexer(lexer);
    let module = parser.parse_module()?;

    let expected_ast = Module {
        declarations: vec![Declaration::Variant(VariantDeclaration {
            name: "result".to_string(),
            cases: vec![
                VariantCase {
                    name: "ok".to_string(),
                    associated_types: vec![Field {
                        name: "value".to_owned(),
                        type_annotation: TypeAnnotationKind::Simple("s32".to_string()),
                    }],
                },
                VariantCase {
                    name: "err".to_string(),
                    associated_types: vec![Field {
                        name: "error".to_owned(),
                        type_annotation: TypeAnnotationKind::Simple("string".to_string()),
                    }],
                },
            ],
        })],
        package: None,
        uses: vec![],
    };

    assert_eq!(module, expected_ast);

    Ok(())
}

#[test]
fn test_parse_variant_declaration_with_mixed_cases() -> Result<()> {
    let input = r#"
    variant response {
        success
        failure(code: s32, message: string)
    }
    "#;

    let lexer = Lexer::new(input);
    let mut parser = Parser::with_lexer(lexer);
    let module = parser.parse_module()?;

    // Assuming that the associated type for 'failure' is a tuple of the types

    let expected_ast = Module {
        declarations: vec![Declaration::Variant(VariantDeclaration {
            name: "response".to_string(),
            cases: vec![
                VariantCase {
                    name: "success".to_string(),
                    associated_types: vec![],
                },
                VariantCase {
                    name: "failure".to_string(),
                    associated_types: vec![
                        Field {
                            name: "code".to_owned(),
                            type_annotation: TypeAnnotationKind::Simple("s32".to_string()),
                        },
                        Field {
                            name: "message".to_owned(),
                            type_annotation: TypeAnnotationKind::Simple("string".to_string()),
                        },
                    ],
                },
            ],
        })],
        package: None,
        uses: vec![],
    };

    assert_eq!(module, expected_ast);

    Ok(())
}

#[test]
fn test_parse_empty_enum_declaration() -> Result<()> {
    let input = r#"
    enum empty_enum {}
    "#;

    let lexer = Lexer::new(input);
    let mut parser = Parser::with_lexer(lexer);

    // Depending on language specifications, an enum with no cases may or may not be allowed.
    // For the purpose of the test, we will assume it is allowed.

    let module = parser.parse_module()?;

    let expected_ast = Module {
        declarations: vec![Declaration::Enum(EnumDeclaration {
            name: "empty_enum".to_string(),
            cases: vec![],
        })],
        package: None,
        uses: vec![],
    };

    assert_eq!(module, expected_ast);

    Ok(())
}
