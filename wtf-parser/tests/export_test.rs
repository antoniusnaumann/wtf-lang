use wtf_ast::*;
use wtf_parser::parser::Parser;
use wtf_parser::parser::Result;

#[cfg(test)]
mod tests {
    use super::*;

    fn parse_module(input: &str) -> Result<Module> {
        let mut parser = Parser::new(input);
        parser.parse_module()
    }

    #[test]
    fn test_export_function() -> Result<()> {
        let source = r#"
        export func greet(name: string) -> string {
            return "Hello, " + name
        }
        "#;

        let expected_ast = Module {
            package: None,
            uses: vec![],
            declarations: vec![Declaration::Export(ExportDeclaration {
                item: Box::new(Declaration::Function(FunctionDeclaration {
                    name: "greet".to_owned(),
                    parameters: vec![Parameter {
                        name: "name".to_owned(),
                        type_annotation: TypeAnnotation::Simple("string".to_owned()),
                    }],
                    return_type: Some(TypeAnnotation::Simple("string".to_owned())),
                    body: Block {
                        statements: vec![Statement::ReturnStatement(Some(
                            Expression::BinaryExpression {
                                left: Box::new(Expression::Literal(Literal::String(
                                    "Hello, ".to_owned(),
                                ))),
                                operator: BinaryOperator::Arithmetic(ArithmeticOperator::Add),
                                right: Box::new(Expression::Identifier("name".to_owned())),
                            },
                        ))],
                    },
                })),
            })],
        };

        let parsed_ast = parse_module(source)?;
        assert_eq!(parsed_ast, expected_ast);

        Ok(())
    }

    #[test]
    fn test_export_record() -> Result<()> {
        let source = r#"
        export record person {
            name: string,
            age: u32,
        }
        "#;

        let expected_ast = Module {
            package: None,
            uses: vec![],
            declarations: vec![Declaration::Export(ExportDeclaration {
                item: Box::new(Declaration::Record(RecordDeclaration {
                    name: "person".to_owned(),
                    fields: vec![
                        Field {
                            name: "name".to_owned(),
                            type_annotation: TypeAnnotation::Simple("string".to_owned()),
                        },
                        Field {
                            name: "age".to_owned(),
                            type_annotation: TypeAnnotation::Simple("u32".to_owned()),
                        },
                    ],
                })),
            })],
        };

        let parsed_ast = parse_module(source)?;
        assert_eq!(parsed_ast, expected_ast);

        Ok(())
    }

    #[test]
    fn test_export_resource() -> Result<()> {
        let source = r#"
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

        let expected_ast = Module {
            package: None,
            uses: vec![],
            declarations: vec![Declaration::Export(ExportDeclaration {
                item: Box::new(Declaration::Resource(ResourceDeclaration {
                    name: "file".to_owned(),
                    fields: vec![Field {
                        name: "path".to_owned(),
                        type_annotation: TypeAnnotation::Simple("string".to_owned()),
                    }],
                    constructor: Some(ConstructorDeclaration {
                        parameters: vec![Parameter {
                            name: "path".to_owned(),
                            type_annotation: TypeAnnotation::Simple("string".to_owned()),
                        }],
                        body: Block {
                            statements: vec![Statement::Assignment {
                                target: Expression::FieldAccess {
                                    object: Box::new(Expression::Identifier("self".to_owned())),
                                    field: "path".to_owned(),
                                    safe: false,
                                },
                                value: Expression::Identifier("path".to_owned()),
                            }],
                        },
                    }),
                    methods: vec![FunctionDeclaration {
                        name: "read".to_owned(),
                        parameters: vec![],
                        return_type: Some(TypeAnnotation::Simple("string".to_owned())),
                        body: Block {
                            statements: vec![
                                // Placeholder for actual logic
                                Statement::ReturnStatement(Some(Expression::Literal(
                                    Literal::String("file content".to_owned()),
                                ))),
                            ],
                        },
                    }],
                })),
            })],
        };

        let parsed_ast = parse_module(source)?;
        assert_eq!(parsed_ast, expected_ast);

        Ok(())
    }

    #[test]
    fn test_export_enum() -> Result<()> {
        let source = r#"
        export enum color {
            red,
            green,
            blue,
        }
        "#;

        let expected_ast = Module {
            package: None,
            uses: vec![],
            declarations: vec![Declaration::Export(ExportDeclaration {
                item: Box::new(Declaration::Enum(EnumDeclaration {
                    name: "color".to_owned(),
                    variants: vec!["red".to_owned(), "green".to_owned(), "blue".to_owned()],
                })),
            })],
        };

        let parsed_ast = parse_module(source)?;
        assert_eq!(parsed_ast, expected_ast);

        Ok(())
    }

    #[test]
    fn test_export_variant() -> Result<()> {
        let source = r#"
        export variant shape {
            circle(radius: f64),
            rectangle(width: f64, height: f64),
        }
        "#;

        let expected_ast = Module {
            package: None,
            uses: vec![],
            declarations: vec![Declaration::Export(ExportDeclaration {
                item: Box::new(Declaration::Variant(VariantDeclaration {
                    name: "shape".to_owned(),
                    cases: vec![
                        VariantCase {
                            name: "circle".to_owned(),
                            associated_types: vec![Field {
                                name: "radius".to_owned(),
                                type_annotation: TypeAnnotation::Simple("f64".to_owned()),
                            }],
                        },
                        VariantCase {
                            name: "rectangle".to_owned(),
                            associated_types: vec![
                                Field {
                                    name: "width".to_owned(),
                                    type_annotation: TypeAnnotation::Simple("f64".to_owned()),
                                },
                                Field {
                                    name: "height".to_owned(),
                                    type_annotation: TypeAnnotation::Simple("f64".to_owned()),
                                },
                            ],
                        },
                    ],
                })),
            })],
        };

        let parsed_ast = parse_module(source)?;
        assert_eq!(parsed_ast, expected_ast);

        Ok(())
    }

    #[test]
    fn test_export_with_package_and_use() -> Result<()> {
        let source = r#"
        package my_module:my_package@1.0.0

        use some_dependency:some_package/some_mod.{some_type, some_other_type}

        export func do_something() {
            // Function body
        }
        "#;

        let expected_ast = Module {
            package: Some(PackageDeclaration {
                path: ModulePath {
                    owner: "my_module".to_owned(),
                    package: "my_package".to_owned(),
                },
                version: Some(Version {
                    major: 1,
                    minor: 0,
                    patch: 0,
                    extras: None,
                }),
            }),
            uses: vec![UseDeclaration {
                module_path: ModulePath {
                    owner: "some_dependency".to_owned(),
                    package: "some_package".to_owned(),
                },
                interface: "some_mod".to_owned(),
                types: vec!["some_type".to_owned(), "some_other_type".to_owned()],
            }],
            declarations: vec![Declaration::Export(ExportDeclaration {
                item: Box::new(Declaration::Function(FunctionDeclaration {
                    name: "do_something".to_owned(),
                    parameters: vec![],
                    return_type: None,
                    body: Block {
                        statements: vec![
                            // Function body placeholder
                        ],
                    },
                })),
            })],
        };

        let parsed_ast = parse_module(source)?;
        assert_eq!(parsed_ast, expected_ast);

        Ok(())
    }
}
