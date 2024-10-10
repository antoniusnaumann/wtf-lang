use wtf_ast::*;
use wtf_parser::lexer::Lexer;
use wtf_parser::parser::{Parser, Result};

#[test]
fn test_parse_record_literal_with_type() -> Result<()> {
    let input = r#"
        func test_function() {
            let point = point {
                x: 1.0,
                y: 2.0
            }
        }
        "#;

    let lexer = Lexer::new(input);
    let mut parser = Parser::with_lexer(lexer);
    let module = parser.parse_module()?;

    let expected_ast = Module {
        declarations: vec![Declaration::Function(FunctionDeclaration {
            name: "test_function".to_string(),
            parameters: vec![],
            return_type: None,
            body: Block {
                statements: vec![Statement::VariableDeclaration(VariableDeclaration {
                    mutable: false,
                    name: "point".to_string(),
                    type_annotation: None,
                    value: Some(Expression::Record {
                        name: Some("point".to_string()),
                        members: vec![
                            FieldAssignment {
                                name: "x".to_string(),
                                element: Expression::Literal(Literal::Float(1.0)),
                            },
                            FieldAssignment {
                                name: "y".to_string(),
                                element: Expression::Literal(Literal::Float(2.0)),
                            },
                        ],
                    }),
                })],
            },
        })],
        package: None,
        uses: vec![],
    };

    assert_eq!(module, expected_ast);

    Ok(())
}

#[test]
fn test_parse_record_literal_without_type() -> Result<()> {
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
    let module = parser.parse_module()?;

    let expected_ast = Module {
        declarations: vec![Declaration::Function(FunctionDeclaration {
            name: "test_function".to_string(),
            parameters: vec![],
            return_type: None,
            body: Block {
                statements: vec![Statement::VariableDeclaration(VariableDeclaration {
                    mutable: false,
                    name: "point".to_string(),
                    type_annotation: None,
                    value: Some(Expression::Record {
                        name: None,
                        members: vec![
                            FieldAssignment {
                                name: "x".to_string(),
                                element: Expression::Literal(Literal::Float(1.0)),
                            },
                            FieldAssignment {
                                name: "y".to_string(),
                                element: Expression::Literal(Literal::Float(2.0)),
                            },
                        ],
                    }),
                })],
            },
        })],
        package: None,
        uses: vec![],
    };

    assert_eq!(module, expected_ast);

    Ok(())
}

#[test]
fn test_parse_nested_record_literal() -> Result<()> {
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
    let module = parser.parse_module()?;

    let expected_ast = Module {
        declarations: vec![Declaration::Function(FunctionDeclaration {
            name: "test_function".to_string(),
            parameters: vec![],
            return_type: None,
            body: Block {
                statements: vec![Statement::VariableDeclaration(VariableDeclaration {
                    mutable: false,
                    name: "person".to_string(),
                    type_annotation: None,
                    value: Some(Expression::Record {
                        name: None,
                        members: vec![
                            FieldAssignment {
                                name: "name".to_string(),
                                element: Expression::Literal(Literal::String("alice".to_string())),
                            },
                            FieldAssignment {
                                name: "address".to_string(),
                                element: Expression::Record {
                                    name: None,
                                    members: vec![
                                        FieldAssignment {
                                            name: "street".to_string(),
                                            element: Expression::Literal(Literal::String(
                                                "main st".to_string(),
                                            )),
                                        },
                                        FieldAssignment {
                                            name: "city".to_string(),
                                            element: Expression::Literal(Literal::String(
                                                "wonderland".to_string(),
                                            )),
                                        },
                                    ],
                                },
                            },
                        ],
                    }),
                })],
            },
        })],
        package: None,
        uses: vec![],
    };

    assert_eq!(module, expected_ast);

    Ok(())
}

#[test]
fn test_parse_record_literal_with_expressions() -> Result<()> {
    let input = r#"
        func test_function() {
            let rectangle = {
                width: 10 * 2,
                height: 5 + 3
            }
        }
        "#;

    let lexer = Lexer::new(input);
    let mut parser = Parser::with_lexer(lexer);
    let module = parser.parse_module()?;

    let expected_ast = Module {
        declarations: vec![Declaration::Function(FunctionDeclaration {
            name: "test_function".to_string(),
            parameters: vec![],
            return_type: None,
            body: Block {
                statements: vec![Statement::VariableDeclaration(VariableDeclaration {
                    mutable: false,
                    name: "rectangle".to_string(),
                    type_annotation: None,
                    value: Some(Expression::Record {
                        name: None,
                        members: vec![
                            FieldAssignment {
                                name: "width".to_string(),
                                element: Expression::BinaryExpression {
                                    left: Box::new(Expression::Literal(Literal::Integer(10))),
                                    operator: BinaryOperator::Arithmetic(
                                        ArithmeticOperator::Multiply,
                                    ),
                                    right: Box::new(Expression::Literal(Literal::Integer(2))),
                                },
                            },
                            FieldAssignment {
                                name: "height".to_string(),
                                element: Expression::BinaryExpression {
                                    left: Box::new(Expression::Literal(Literal::Integer(5))),
                                    operator: BinaryOperator::Arithmetic(ArithmeticOperator::Add),
                                    right: Box::new(Expression::Literal(Literal::Integer(3))),
                                },
                            },
                        ],
                    }),
                })],
            },
        })],
        package: None,
        uses: vec![],
    };

    assert_eq!(module, expected_ast);

    Ok(())
}

#[test]
fn test_parse_record_literal_with_type_and_expressions() -> Result<()> {
    let input = r#"
        func test_function() {
            let circle = shape {
                radius: 5.0,
                area: pi * (5.0 * 5.0)
            }
        }
        "#;

    let lexer = Lexer::new(input);
    let mut parser = Parser::with_lexer(lexer);
    let module = parser.parse_module()?;

    let expected_ast = Module {
        declarations: vec![Declaration::Function(FunctionDeclaration {
            name: "test_function".to_string(),
            parameters: vec![],
            return_type: None,
            body: Block {
                statements: vec![Statement::VariableDeclaration(VariableDeclaration {
                    mutable: false,
                    name: "circle".to_string(),
                    type_annotation: None,
                    value: Some(Expression::Record {
                        name: Some("shape".to_string()),
                        members: vec![
                            FieldAssignment {
                                name: "radius".to_string(),
                                element: Expression::Literal(Literal::Float(5.0)),
                            },
                            FieldAssignment {
                                name: "area".to_string(),
                                element: Expression::BinaryExpression {
                                    left: Box::new(Expression::Identifier("pi".to_string())),
                                    operator: BinaryOperator::Arithmetic(
                                        ArithmeticOperator::Multiply,
                                    ),
                                    right: Box::new(Expression::BinaryExpression {
                                        left: Box::new(Expression::Literal(Literal::Float(5.0))),
                                        operator: BinaryOperator::Arithmetic(
                                            ArithmeticOperator::Multiply,
                                        ),
                                        right: Box::new(Expression::Literal(Literal::Float(5.0))),
                                    }),
                                },
                            },
                        ],
                    }),
                })],
            },
        })],
        package: None,
        uses: vec![],
    };

    assert_eq!(module, expected_ast);

    Ok(())
}

#[test]
fn test_parse_record_literal_with_list_field() -> Result<()> {
    let input = r#"
        func test_function() {
            let data = {
                values: [1, 2, 3, 4],
                name: "sample data"
            }
        }
        "#;

    let lexer = Lexer::new(input);
    let mut parser = Parser::with_lexer(lexer);
    let module = parser.parse_module()?;

    let expected_ast = Module {
        declarations: vec![Declaration::Function(FunctionDeclaration {
            name: "test_function".to_string(),
            parameters: vec![],
            return_type: None,
            body: Block {
                statements: vec![Statement::VariableDeclaration(VariableDeclaration {
                    mutable: false,
                    name: "data".to_string(),
                    type_annotation: None,
                    value: Some(Expression::Record {
                        name: None,
                        members: vec![
                            FieldAssignment {
                                name: "values".to_string(),
                                element: Expression::ListLiteral(vec![
                                    Expression::Literal(Literal::Integer(1)),
                                    Expression::Literal(Literal::Integer(2)),
                                    Expression::Literal(Literal::Integer(3)),
                                    Expression::Literal(Literal::Integer(4)),
                                ]),
                            },
                            FieldAssignment {
                                name: "name".to_string(),
                                element: Expression::Literal(Literal::String(
                                    "sample data".to_string(),
                                )),
                            },
                        ],
                    }),
                })],
            },
        })],
        package: None,
        uses: vec![],
    };

    assert_eq!(module, expected_ast);

    Ok(())
}

#[test]
fn test_parse_empty_record_literal() -> Result<()> {
    let input = r#"
        func test_function() {
            let empty = {}
        }
        "#;

    let lexer = Lexer::new(input);
    let mut parser = Parser::with_lexer(lexer);
    let module = parser.parse_module()?;

    let expected_ast = Module {
        declarations: vec![Declaration::Function(FunctionDeclaration {
            name: "test_function".to_string(),
            parameters: vec![],
            return_type: None,
            body: Block {
                statements: vec![Statement::VariableDeclaration(VariableDeclaration {
                    mutable: false,
                    name: "empty".to_string(),
                    type_annotation: None,
                    value: Some(Expression::Record {
                        name: None,
                        members: vec![],
                    }),
                })],
            },
        })],
        package: None,
        uses: vec![],
    };

    assert_eq!(module, expected_ast);

    Ok(())
}

#[test]
#[ignore = "TODO: Implement spreading and update this test"]
fn test_parse_record_literal_with_spread() -> Result<()> {
    let input = r#"
        func test_function() {
            let point3d = {
                ...point,
                z: 3.0
            }
        }
        "#;

    // Assuming that the parser and AST support spread syntax

    let lexer = Lexer::new(input);
    let mut parser = Parser::with_lexer(lexer);
    let module = parser.parse_module()?;

    // The expected AST needs to account for the spread operator
    let expected_ast = Module {
        declarations: vec![Declaration::Function(FunctionDeclaration {
            name: "test_function".to_string(),
            parameters: vec![],
            return_type: None,
            body: Block {
                statements: vec![Statement::VariableDeclaration(VariableDeclaration {
                    mutable: false,
                    name: "point3d".to_string(),
                    type_annotation: None,
                    value: Some(Expression::Record {
                        name: None,
                        members: vec![
                            // Assuming we have a FieldAssignment variant for spread
                            FieldAssignment {
                                name: "...".to_string(),
                                element: Expression::Identifier("point".to_string()),
                            },
                            FieldAssignment {
                                name: "z".to_string(),
                                element: Expression::Literal(Literal::Float(3.0)),
                            },
                        ],
                    }),
                })],
            },
        })],
        package: None,
        uses: vec![],
    };

    assert_eq!(module, expected_ast);

    Ok(())
}
