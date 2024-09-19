use wtf_ast::*;
use wtf_parser::lexer::Lexer;
use wtf_parser::parser::{Parser, Result};

#[test]
fn test_parse_optional_chaining() -> Result<()> {
    let input = r#"
    func test_optional_chaining() {
        let value = optional_value?.to_string()
    }
    "#;

    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);
    let module = parser.parse_module()?;

    let expected_ast = Module {
        declarations: vec![Declaration::Function(FunctionDeclaration {
            name: "test_optional_chaining".to_string(),
            parameters: vec![],
            return_type: None,
            body: Block {
                statements: vec![Statement::VariableDeclaration(VariableDeclaration {
                    mutable: false,
                    name: "value".to_string(),
                    type_annotation: None,
                    value: Expression::MethodCall {
                        method: "to_string".to_string(),
                        arguments: vec![],
                        safe: true,
                        receiver: Expression::Identifier("optional_value".to_owned()).into(),
                    },
                })],
            },
        })],
    };

    assert_eq!(module, expected_ast);

    Ok(())
}
