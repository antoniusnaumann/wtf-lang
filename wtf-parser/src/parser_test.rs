#[cfg(test)]
mod tests {
    use crate::lexer::Lexer;
    use crate::parser::{Parser, Result};

    use wtf_ast::*;

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
    fn test_parse_if_statement() -> Result<()> {
        let input = r#"
        func check_value(x: s32) {
            if x > 10 {
                println("Greater than 10")
            } else {
                println("10 or less")
            }
        }
        "#;

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let module = parser.parse_module()?;

        let expected_ast = Module {
            declarations: vec![Declaration::Function(FunctionDeclaration {
                name: "check_value".to_string(),
                parameters: vec![Parameter {
                    name: "x".to_string(),
                    type_annotation: TypeAnnotation::Simple("s32".to_string()),
                }],
                return_type: None,
                body: Block {
                    statements: vec![Statement::IfStatement(IfStatement {
                        condition: Expression::BinaryExpression {
                            left: Box::new(Expression::Identifier("x".to_string())),
                            operator: BinaryOperator::GreaterThan,
                            right: Box::new(Expression::Literal(Literal::Integer(10))),
                        },
                        then_branch: Block {
                            statements: vec![Statement::ExpressionStatement(
                                Expression::FunctionCall {
                                    function: Box::new(Expression::Identifier(
                                        "println".to_string(),
                                    )),
                                    arguments: vec![Expression::Literal(Literal::String(
                                        "Greater than 10".to_string(),
                                    ))],
                                },
                            )],
                        },
                        else_branch: Some(Block {
                            statements: vec![Statement::ExpressionStatement(
                                Expression::FunctionCall {
                                    function: Box::new(Expression::Identifier(
                                        "println".to_string(),
                                    )),
                                    arguments: vec![Expression::Literal(Literal::String(
                                        "10 or less".to_string(),
                                    ))],
                                },
                            )],
                        }),
                    })],
                },
            })],
        };

        assert_eq!(module, expected_ast);

        Ok(())
    }

    #[test]
    fn test_parse_return_statement() -> Result<()> {
        let input = r#"
        func get_value() -> s32 {
            return 42
        }
        "#;

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let module = parser.parse_module()?;

        let expected_ast = Module {
            declarations: vec![Declaration::Function(FunctionDeclaration {
                name: "get_value".to_string(),
                parameters: vec![],
                return_type: Some(TypeAnnotation::Simple("s32".to_string())),
                body: Block {
                    statements: vec![Statement::ReturnStatement(Some(Expression::Literal(
                        Literal::Integer(42),
                    )))],
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
                            },
                            value: Expression::BinaryExpression {
                                left: Box::new(Expression::FieldAccess {
                                    object: Box::new(Expression::Identifier("self".to_string())),
                                    field: "value".to_string(),
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
                        },
                        value: Expression::BinaryExpression {
                            left: Box::new(Expression::FieldAccess {
                                object: Box::new(Expression::Identifier("self".to_string())),
                                field: "value".to_string(),
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
        let mut parser = Parser::new(lexer);
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
                        value: Expression::Record {
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
                        },
                    })],
                },
            })],
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
        let mut parser = Parser::new(lexer);
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
                        value: Expression::Record {
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
                        },
                    })],
                },
            })],
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
        let mut parser = Parser::new(lexer);
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
                        value: Expression::Record {
                            name: None,
                            members: vec![
                                FieldAssignment {
                                    name: "name".to_string(),
                                    element: Expression::Literal(Literal::String(
                                        "alice".to_string(),
                                    )),
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
                        },
                    })],
                },
            })],
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
        let mut parser = Parser::new(lexer);
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
                        value: Expression::Record {
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
                                        operator: BinaryOperator::Arithmetic(
                                            ArithmeticOperator::Add,
                                        ),
                                        right: Box::new(Expression::Literal(Literal::Integer(3))),
                                    },
                                },
                            ],
                        },
                    })],
                },
            })],
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
        let mut parser = Parser::new(lexer);
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
                        value: Expression::Record {
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
                                            left: Box::new(Expression::Literal(Literal::Float(
                                                5.0,
                                            ))),
                                            operator: BinaryOperator::Arithmetic(
                                                ArithmeticOperator::Multiply,
                                            ),
                                            right: Box::new(Expression::Literal(Literal::Float(
                                                5.0,
                                            ))),
                                        }),
                                    },
                                },
                            ],
                        },
                    })],
                },
            })],
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
        let mut parser = Parser::new(lexer);
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
                        value: Expression::Record {
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
                        },
                    })],
                },
            })],
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
        let mut parser = Parser::new(lexer);
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
                        value: Expression::Record {
                            name: None,
                            members: vec![],
                        },
                    })],
                },
            })],
        };

        assert_eq!(module, expected_ast);

        Ok(())
    }
}
