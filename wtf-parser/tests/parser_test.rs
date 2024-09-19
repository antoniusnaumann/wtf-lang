use wtf_ast::*;
use wtf_parser::lexer::Lexer;
use wtf_parser::parser::{Parser, Result};

#[test]
fn test_parse_function_declaration() {
    let input = r#"
        func add(a: s32, b: s32) -> s32 {
            return a + b
        }
        "#;

    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);
    let module = parser.parse_module().unwrap();

    let expected_ast = Module {
        declarations: vec![Declaration::Function(FunctionDeclaration {
            name: "add".to_string(),
            parameters: vec![
                Parameter {
                    name: "a".to_string(),
                    type_annotation: TypeAnnotation::Simple("s32".to_string()),
                },
                Parameter {
                    name: "b".to_string(),
                    type_annotation: TypeAnnotation::Simple("s32".to_string()),
                },
            ],
            return_type: Some(TypeAnnotation::Simple("s32".to_string())),
            body: Block {
                statements: vec![Statement::ReturnStatement(Some(
                    Expression::BinaryExpression {
                        left: Box::new(Expression::Identifier("a".to_string())),
                        operator: ArithmeticOperator::Add.into(),
                        right: Box::new(Expression::Identifier("b".to_string())),
                    },
                ))],
            },
        })],
    };

    assert_eq!(module, expected_ast);
}

#[test]
fn test_parse_variable_declaration() -> Result<()> {
    let input = r#"
        func main() {
            let x = 42
            var y: s32 = x + 1
        }
        "#;

    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);
    let module = parser.parse_module()?;

    let expected_ast = Module {
        declarations: vec![Declaration::Function(FunctionDeclaration {
            name: "main".to_string(),
            parameters: vec![],
            return_type: None,
            body: Block {
                statements: vec![
                    Statement::VariableDeclaration(VariableDeclaration {
                        mutable: false,
                        name: "x".to_string(),
                        type_annotation: None,
                        value: Expression::Literal(Literal::Integer(42)),
                    }),
                    Statement::VariableDeclaration(VariableDeclaration {
                        mutable: true,
                        name: "y".to_string(),
                        type_annotation: Some(TypeAnnotation::Simple("s32".to_string())),
                        value: Expression::BinaryExpression {
                            left: Box::new(Expression::Identifier("x".to_string())),
                            operator: ArithmeticOperator::Add.into(),
                            right: Box::new(Expression::Literal(Literal::Integer(1))),
                        },
                    }),
                ],
            },
        })],
    };

    assert_eq!(module, expected_ast);

    Ok(())
}

#[test]
fn test_parse_resource_declaration() -> Result<()> {
    let input = r#"
        resource counter {
            value: s32

            constructor(initial: s32) {
                self.value = initial
            }

            func increment() {
                self.value = self.value + 1
            }
        }
        "#;

    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);
    let module = parser.parse_module()?;

    let expected_ast = Module {
        declarations: vec![Declaration::Resource(ResourceDeclaration {
            name: "counter".to_string(),
            fields: vec![Field {
                name: "value".to_string(),
                type_annotation: TypeAnnotation::Simple("s32".to_string()),
            }],
            constructor: Some(ConstructorDeclaration {
                parameters: vec![Parameter {
                    name: "initial".to_string(),
                    type_annotation: TypeAnnotation::Simple("s32".to_string()),
                }],
                body: Block {
                    statements: vec![Statement::Assignment {
                        target: Expression::FieldAccess {
                            object: Box::new(Expression::Identifier("self".to_string())),
                            field: "value".to_string(),
                            safe: false,
                        },
                        value: Expression::Identifier("initial".to_string()),
                    }],
                },
            }),
            methods: vec![FunctionDeclaration {
                name: "increment".to_string(),
                parameters: vec![],
                return_type: None,
                body: Block {
                    statements: vec![Statement::Assignment {
                        target: Expression::FieldAccess {
                            object: Box::new(Expression::Identifier("self".to_string())),
                            field: "value".to_string(),
                            safe: false,
                        },
                        value: Expression::BinaryExpression {
                            left: Box::new(Expression::FieldAccess {
                                object: Box::new(Expression::Identifier("self".to_string())),
                                field: "value".to_string(),
                                safe: false,
                            }),
                            operator: ArithmeticOperator::Add.into(),
                            right: Box::new(Expression::Literal(Literal::Integer(1))),
                        },
                    }],
                },
            }],
        })],
    };

    assert_eq!(module, expected_ast);

    Ok(())
}

#[test]
fn test_parse_plus_assign() -> Result<()> {
    let input = r#"
            func increment() {
                self.value += 1
            }
        "#;

    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);
    let module = parser.parse_module()?;

    let expected_ast = Module {
        declarations: vec![Declaration::Function(FunctionDeclaration {
            name: "increment".to_string(),
            parameters: vec![],
            return_type: None,
            body: Block {
                statements: vec![Statement::Assignment {
                    target: Expression::FieldAccess {
                        object: Box::new(Expression::Identifier("self".to_string())),
                        field: "value".to_string(),
                        safe: false,
                    },
                    value: Expression::BinaryExpression {
                        left: Box::new(Expression::FieldAccess {
                            object: Box::new(Expression::Identifier("self".to_string())),
                            field: "value".to_string(),
                            safe: false,
                        }),
                        operator: ArithmeticOperator::Add.into(),
                        right: Box::new(Expression::Literal(Literal::Integer(1))),
                    },
                }],
            },
        })],
    };

    assert_eq!(module, expected_ast);

    Ok(())
}

#[test]
fn test_parse_package_declaration() -> Result<()> {
    let input = r#"
        package test:all_features
        "#;

    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);
    let module = parser.parse_module()?;

    let expected_ast = Module {
        declarations: vec![Declaration::Package(PackageDeclaration {
            path: ModulePath {
                owner: "test".to_owned(),
                package: "all_features".to_owned(),
            },
            version: None,
        })],
    };

    assert_eq!(module, expected_ast);

    Ok(())
}

#[test]
fn test_parse_package_declaration_versioned() -> Result<()> {
    let input = r#"
        package test:all_features@1.0.0
        "#;

    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);
    let module = parser.parse_module()?;

    let expected_ast = Module {
        declarations: vec![Declaration::Package(PackageDeclaration {
            path: ModulePath {
                owner: "test".to_owned(),
                package: "all_features".to_owned(),
            },
            version: Some(Version {
                major: 1,
                minor: 0,
                patch: 0,
                extras: None,
            }),
        })],
    };

    assert_eq!(module, expected_ast);

    Ok(())
}

#[test]
fn test_parse_use_declaration() -> Result<()> {
    let input = r#"
        use external:io/printer.{print}
        "#;

    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);
    let module = parser.parse_module()?;

    let expected_ast = Module {
        declarations: vec![Declaration::Use(UseDeclaration {
            module_path: ModulePath {
                owner: "external".to_owned(),
                package: "io".to_owned(),
            },
            interface: "printer".to_owned(),
            types: vec!["print".to_owned()],
        })],
    };

    assert_eq!(module, expected_ast);

    Ok(())
}

#[test]
fn test_parse_use_declaration_two_imports() -> Result<()> {
    let input = r#"
        use external:io/printer.{print, println}
        "#;

    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);
    let module = parser.parse_module()?;

    let expected_ast = Module {
        declarations: vec![Declaration::Use(UseDeclaration {
            module_path: ModulePath {
                owner: "external".to_owned(),
                package: "io".to_owned(),
            },
            interface: "printer".to_owned(),
            types: vec!["print".to_owned(), "println".to_owned()],
        })],
    };

    assert_eq!(module, expected_ast);

    Ok(())
}
