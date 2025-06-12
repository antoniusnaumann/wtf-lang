use wtf_ast::*;
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

    let expected_ast = Module {
        declarations: vec![Declaration::Function(FunctionDeclaration {
            name: "add".to_string(),
            parameters: vec![
                Parameter {
                    name: "a".to_string(),
                    type_annotation: TypeAnnotationKind::Simple("s32".to_string()),
                },
                Parameter {
                    name: "b".to_string(),
                    type_annotation: TypeAnnotationKind::Simple("s32".to_string()),
                },
            ],
            return_type: Some(TypeAnnotationKind::Simple("s32".to_string())),
            body: Block {
                statements: vec![Statement::ReturnStatement(Some(
                    Expression::BinaryExpression {
                        left: Expression::Identifier("a".to_string()).into(),
                        operator: ArithmeticOperator::Add.into(),
                        right: Expression::Identifier("b".to_string()).into(),
                    },
                ))],
            },
        })],
        package: None,
        uses: vec![],
    };

    assert_eq!(module, expected_ast);

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

    let expected_ast = Module {
        declarations: vec![Declaration::Function(FunctionDeclaration {
            name: "something".to_string(),
            parameters: vec![],
            return_type: None,
            body: Block {
                statements: vec![
                    Statement::IfStatement(IfStatement {
                        condition: Expression::Literal(Literal::Boolean(true)),
                        then_branch: Block { statements: vec![] },
                        else_branch: None,
                    }),
                    Statement::ReturnStatement(Some(Expression::BinaryExpression {
                        left: Expression::Identifier("a".to_string()).into(),
                        operator: ArithmeticOperator::Add.into(),
                        right: Expression::Identifier("b".to_string()).into(),
                    })),
                ],
            },
        })],
        package: None,
        uses: vec![],
    };

    assert_eq!(module, expected_ast);

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

    let expected_ast = Module {
        declarations: vec![Declaration::Function(FunctionDeclaration {
            name: "something".to_string(),
            parameters: vec![],
            return_type: None,
            body: Block {
                statements: vec![
                    Statement::IfStatement(IfStatement {
                        condition: Expression::Literal(Literal::Boolean(true)),
                        then_branch: Block { statements: vec![] },
                        else_branch: None,
                    }),
                    Statement::ReturnStatement(Some(Expression::BinaryExpression {
                        left: Expression::Identifier("a".to_string()).into(),
                        operator: ArithmeticOperator::Add.into(),
                        right: Expression::Identifier("b".to_string()).into(),
                    })),
                ],
            },
        })],
        package: None,
        uses: vec![],
    };

    assert_eq!(module, expected_ast);

    assert_eq!(parser.errors().len(), 1);
}
