use wtf_ast::*;
use wtf_parser::lexer::Lexer;
use wtf_parser::parser::{Parser, Result};

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
    let mut parser = Parser::with_lexer(lexer);
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
                                function: Box::new(Expression::Identifier("println".to_string())),
                                arguments: vec![Expression::Literal(Literal::String(
                                    "Greater than 10".to_string(),
                                ))],
                            },
                        )],
                    },
                    else_branch: Some(Block {
                        statements: vec![Statement::ExpressionStatement(
                            Expression::FunctionCall {
                                function: Box::new(Expression::Identifier("println".to_string())),
                                arguments: vec![Expression::Literal(Literal::String(
                                    "10 or less".to_string(),
                                ))],
                            },
                        )],
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
fn test_parse_return_statement() -> Result<()> {
    let input = r#"
        func get_value() -> s32 {
            return 42
        }
        "#;

    let lexer = Lexer::new(input);
    let mut parser = Parser::with_lexer(lexer);
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
        package: None,
        uses: vec![],
    };

    assert_eq!(module, expected_ast);

    Ok(())
}

#[test]
fn test_parse_throw_statement() -> Result<()> {
    let input = r#"
        func get_value() -> s32! {
            throw "Not implemented"
        }
        "#;

    let lexer = Lexer::new(input);
    let mut parser = Parser::with_lexer(lexer);
    let module = parser.parse_module()?;

    let expected_ast = Module {
        declarations: vec![Declaration::Function(FunctionDeclaration {
            name: "get_value".to_string(),
            parameters: vec![],
            return_type: Some(TypeAnnotation::Result {
                ok: TypeAnnotation::Simple("s32".to_owned()).into(),
                err: TypeAnnotation::Simple("error".to_owned()).into(),
            }),
            body: Block {
                statements: vec![Statement::ThrowStatement(Expression::Literal(
                    Literal::String("Not implemented".to_owned()),
                ))],
            },
        })],
        package: None,
        uses: vec![],
    };

    assert_eq!(module, expected_ast);

    Ok(())
}
