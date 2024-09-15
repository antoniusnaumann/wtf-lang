// parser.rs

use crate::ast::*;
use crate::lexer::{Lexer, SpannedToken, Token};

#[derive(Clone, Debug)]
pub struct ParserError {
    found: SpannedToken,
    expected: Vec<Token>,
}

pub type Result<Ok> = std::result::Result<Ok, ParserError>;

pub struct Parser {
    lexer: Lexer,
    current: SpannedToken,
}

impl Parser {
    pub fn new(mut lexer: Lexer) -> Self {
        let current = lexer.next_token();
        Parser { lexer, current }
    }

    fn advance_tokens(&mut self) {
        self.current = self.lexer.next_token();
    }

    fn expect_token(&mut self, expected: Token) -> Result<()> {
        if self.has(expected.clone()) {
            self.advance_tokens();
            Ok(())
        } else {
            Err(ParserError {
                found: self.current.clone(),
                expected: vec![expected],
            })
        }
    }

    /// Produces an appropriate parser error for an unexpected token
    fn unexpected(&self, expected: Vec<Token>) -> ParserError {
        ParserError {
            found: self.current.clone(),
            expected,
        }
    }

    fn skip_newline(&mut self) {
        while self.current.token == Token::Newline {
            self.advance_tokens();
        }
    }

    fn has(&self, expected: Token) -> bool {
        matches!(&self.current, SpannedToken { token, .. } if *token == expected)
    }

    fn current_token_as_identifier(&self) -> Option<String> {
        match &self.current {
            SpannedToken {
                token: Token::Identifier(name),
                ..
            } => Some(name.to_owned()),
            _ => None,
        }
    }

    pub fn parse_module(&mut self) -> Result<Module> {
        let mut declarations = Vec::new();

        while self.current.token != Token::Eof {
            match self.parse_declaration() {
                Ok(decl) => {
                    declarations.push(decl);
                }
                Err(err) => {
                    if self.current.token != Token::Newline {
                        return Err(err);
                    }

                    self.advance_tokens();
                }
            }
        }

        Ok(Module { declarations })
    }

    fn parse_declaration(&mut self) -> Result<Declaration> {
        match &self.current.token {
            Token::Func => self.parse_function_declaration().map(Declaration::Function),
            Token::Record => self.parse_record_declaration().map(Declaration::Record),
            Token::Resource => self.parse_resource_declaration().map(Declaration::Resource),
            Token::Enum => self.parse_enum_declaration().map(Declaration::Enum),
            Token::Variant => self.parse_variant_declaration().map(Declaration::Variant),
            Token::Import => self.parse_import_declaration().map(Declaration::Import),
            Token::Export => self.parse_export_declaration().map(Declaration::Export),
            Token::Package => self.parse_package_declaration().map(Declaration::Package),
            Token::Use => self.parse_use_declaration().map(Declaration::Use),
            _ => Err(self.unexpected(vec![
                Token::Func,
                Token::Record,
                Token::Resource,
                Token::Enum,
                Token::Variant,
                Token::Import,
                Token::Export,
                Token::Package,
                Token::Use,
            ])),
        }
    }

    fn parse_function_declaration(&mut self) -> Result<FunctionDeclaration> {
        self.expect_token(Token::Func)?;

        let name = self.expect_identifier()?;

        self.expect_token(Token::LeftParen)?;

        let parameters = self.parse_parameters()?;

        self.expect_token(Token::RightParen)?;

        let return_type = if self.has(Token::Arrow) {
            self.advance_tokens();
            Some(self.parse_type_annotation()?)
        } else {
            None
        };

        let body = self.parse_block()?;

        Ok(FunctionDeclaration {
            name,
            parameters,
            return_type,
            body,
        })
    }

    fn parse_parameters(&mut self) -> Result<Vec<Parameter>> {
        let mut parameters = Vec::new();

        if self.has(Token::RightParen) {
            return Ok(parameters);
        }

        loop {
            let name = self.expect_identifier()?;
            self.expect_token(Token::Colon)?;
            let type_annotation = self.parse_type_annotation()?;
            parameters.push(Parameter {
                name,
                type_annotation,
            });

            if self.has(Token::Comma) {
                self.advance_tokens();
            } else if self.has(Token::RightParen) {
                break;
            } else {
                // TODO: Error handling
                return Err(self.unexpected(vec![Token::Comma, Token::RightParen]));
            }
        }

        Ok(parameters)
    }

    fn parse_type_annotation(&mut self) -> Result<TypeAnnotation> {
        if let Token::Identifier(name) = self.current.token.clone() {
            let type_name = name.clone();
            self.advance_tokens();

            match type_name.as_str() {
                "list" => {
                    self.expect_token(Token::LessThan)?;
                    let inner_type = self.parse_type_annotation()?;
                    self.expect_token(Token::GreaterThan)?;
                    Ok(TypeAnnotation::List(Box::new(inner_type)))
                }
                "option" => {
                    self.expect_token(Token::LessThan)?;
                    let inner_type = self.parse_type_annotation()?;
                    self.expect_token(Token::GreaterThan)?;
                    Ok(TypeAnnotation::Option(Box::new(inner_type)))
                }
                "result" => {
                    self.expect_token(Token::LessThan)?;
                    let ok_type = self.parse_type_annotation()?;
                    self.expect_token(Token::Comma)?;
                    let err_type = self.parse_type_annotation()?;
                    self.expect_token(Token::GreaterThan)?;
                    Ok(TypeAnnotation::Result {
                        ok_type: Box::new(ok_type),
                        err_type: Box::new(err_type),
                    })
                }
                _ => Ok(TypeAnnotation::Simple(type_name)),
            }
        } else {
            // TODO: Add expected ast nodes
            Err(self.unexpected(vec![]))
        }
    }

    fn parse_block(&mut self) -> Result<Block> {
        self.expect_token(Token::LeftBrace)?;

        let mut statements = Vec::new();

        while !self.has(Token::RightBrace) && !self.has(Token::Eof) {
            if let Ok(stmt) = self.parse_statement() {
                statements.push(stmt);
            } else {
                self.advance_tokens();
            }
        }

        self.expect_token(Token::RightBrace)?;

        Ok(Block { statements })
    }

    fn parse_statement(&mut self) -> Result<Statement> {
        match self.current.token {
            Token::Let | Token::Var => self
                .parse_variable_declaration()
                .map(Statement::VariableDeclaration),
            Token::If => self.parse_if_statement().map(Statement::IfStatement),
            Token::Return => self.parse_return_statement(),
            Token::Throw => self.parse_throw_statement(),
            Token::Break => {
                self.advance_tokens();
                Ok(Statement::BreakStatement(None)) // No value for break
            }
            Token::Continue => {
                self.advance_tokens();
                Ok(Statement::ContinueStatement)
            }
            Token::While => self.parse_while_statement().map(Statement::WhileStatement),
            Token::For => self.parse_for_statement().map(Statement::ForStatement),
            Token::Match => self.parse_match_statement().map(Statement::MatchStatement),
            _ => {
                let expr = self.parse_expression()?;

                if let Some(op) = self.current_token_as_assignment() {
                    // TODO: Check that target is a valid assignment target
                    self.advance_tokens();
                    let value = self.parse_expression()?;
                    Ok(Statement::Assignment {
                        target: expr,
                        op,
                        value,
                    })
                } else {
                    Ok(Statement::ExpressionStatement(expr))
                }
            }
        }
    }

    fn parse_variable_declaration(&mut self) -> Result<VariableDeclaration> {
        let mutable = self.has(Token::Var);
        self.advance_tokens();

        let name = self.expect_identifier()?;

        let type_annotation = if self.has(Token::Colon) {
            self.advance_tokens();
            Some(self.parse_type_annotation()?)
        } else {
            None
        };

        self.expect_token(Token::Equal)?;

        let value = self.parse_expression()?;

        Ok(VariableDeclaration {
            mutable,
            name,
            type_annotation,
            value,
        })
    }

    fn parse_if_statement(&mut self) -> Result<IfStatement> {
        self.expect_token(Token::If)?;

        let condition = self.parse_expression()?;

        let then_branch = self.parse_block()?;

        let else_branch = if self.has(Token::Else) {
            self.advance_tokens();
            Some(self.parse_block()?)
        } else {
            None
        };

        Ok(IfStatement {
            condition,
            then_branch,
            else_branch,
        })
    }

    fn parse_while_statement(&mut self) -> Result<WhileStatement> {
        self.expect_token(Token::While)?;

        let condition = self.parse_expression()?;

        let body = self.parse_block()?;

        Ok(WhileStatement { condition, body })
    }

    fn parse_for_statement(&mut self) -> Result<ForStatement> {
        self.expect_token(Token::For)?;

        let variable = self.expect_identifier()?;

        self.expect_token(Token::Contains)?;

        let iterable = self.parse_expression()?;

        let body = self.parse_block()?;

        Ok(ForStatement {
            variable,
            iterable,
            body,
        })
    }

    fn parse_match_statement(&mut self) -> Result<MatchStatement> {
        self.expect_token(Token::Match)?;

        let expression = self.parse_expression()?;

        self.expect_token(Token::LeftBrace)?;

        let mut arms = Vec::new();

        while !self.has(Token::RightBrace) && !self.has(Token::Eof) {
            let pattern = self.parse_pattern()?;
            self.expect_token(Token::Arrow)?;
            let body = if self.has(Token::LeftBrace) {
                self.parse_block()?
            } else {
                let expr = self.parse_expression()?;
                Block {
                    statements: vec![Statement::ExpressionStatement(expr)],
                }
            };
            arms.push(MatchArm { pattern, body });
            // Optional: Handle comma or semicolon between arms
        }

        self.expect_token(Token::RightBrace)?;

        Ok(MatchStatement { expression, arms })
    }

    fn parse_pattern(&mut self) -> Result<Pattern> {
        if let Some(name) = self.current_token_as_identifier() {
            self.advance_tokens();
            if self.has(Token::LeftParen) {
                self.advance_tokens();
                let sub_pattern = self.parse_pattern()?;
                self.expect_token(Token::RightParen)?;
                Ok(Pattern::VariantPattern {
                    variant_name: name.into(),
                    sub_pattern: Some(Box::new(sub_pattern)),
                })
            } else {
                Ok(Pattern::Identifier(name.into()))
            }
        } else if let Ok(literal) = self.parse_literal() {
            Ok(Pattern::Literal(literal))
        } else {
            // TODO: Add similar parser error where AST nodes can be expected
            Err(self.unexpected(vec![
                Token::Identifier("".to_owned()),
                Token::LeftParen,
                Token::IntegerLiteral(0),
                Token::FloatLiteral(0.0),
                Token::StringLiteral("".to_owned()),
            ]))
        }
    }

    fn parse_return_statement(&mut self) -> Result<Statement> {
        self.expect_token(Token::Return)?;

        let expression = if !self.has(Token::Newline) && !self.has(Token::RightBrace) {
            Some(self.parse_expression()?)
        } else {
            None
        };

        Ok(Statement::ReturnStatement(expression))
    }

    fn parse_throw_statement(&mut self) -> Result<Statement> {
        self.expect_token(Token::Throw)?;

        let expression = self.parse_expression()?;

        Ok(Statement::ThrowStatement(expression))
    }

    fn parse_expression(&mut self) -> Result<Expression> {
        self.parse_binary_expression(0)
    }

    fn parse_binary_expression(&mut self, precedence: u8) -> Result<Expression> {
        let mut left = self.parse_unary_expression()?;

        while let Some(op_precedence) = self.get_precedence() {
            if op_precedence < precedence {
                break;
            }

            let Some(operator) = self.current_token_as_binary_operator() else {
                // TODO: Add Vec with all operators
                return Err(self.unexpected(vec![]));
            };
            self.advance_tokens();

            let right = self.parse_binary_expression(op_precedence + 1)?;
            left = Expression::BinaryExpression {
                left: Box::new(left),
                operator,
                right: Box::new(right),
            };
        }

        Ok(left)
    }

    fn parse_unary_expression(&mut self) -> Result<Expression> {
        if let Some(operator) = self.current_token_as_unary_operator() {
            self.advance_tokens();
            let operand = self.parse_unary_expression()?;
            Ok(Expression::UnaryExpression {
                operator,
                operand: Box::new(operand),
            })
        } else {
            let mut expr = self.parse_primary_expression()?;

            // Handle postfix operators: '!' (yeet operator)
            if self.has(Token::Bang) {
                self.advance_tokens();
                expr = Expression::YeetExpression {
                    expression: Box::new(expr),
                };
            }

            Ok(expr)
        }
    }

    fn parse_primary_expression(&mut self) -> Result<Expression> {
        match &self.current.token {
            Token::IntegerLiteral(value) => {
                let expr = Expression::Literal(Literal::Integer(*value));
                self.advance_tokens();
                Ok(expr)
            }
            Token::FloatLiteral(value) => {
                let expr = Expression::Literal(Literal::Float(*value));
                self.advance_tokens();
                Ok(expr)
            }
            Token::StringLiteral(value) => {
                let expr = Expression::Literal(Literal::String(value.clone()));
                self.advance_tokens();
                Ok(expr)
            }
            Token::Identifier(name) => {
                let name = name.clone();
                self.advance_tokens();
                self.parse_postfix_expression(Expression::Identifier(name))
            }
            Token::LeftParen => {
                self.advance_tokens();
                let expr = self.parse_expression()?;
                self.expect_token(Token::RightParen)?;
                Ok(expr)
            }
            _ => Err(self.unexpected(vec![])),
        }
    }

    fn parse_postfix_expression(&mut self, expr: Expression) -> Result<Expression> {
        let mut result = expr;

        loop {
            if self.has(Token::LeftParen) {
                result = self.parse_function_call(result)?;
            } else if self.has(Token::Dot) {
                self.advance_tokens();
                let field = self.expect_identifier()?;
                result = Expression::FieldAccess {
                    object: Box::new(result),
                    field,
                };
            } else if self.has(Token::SafeCall) {
                self.advance_tokens();
                let field = self.expect_identifier()?;
                result = Expression::SafeFieldAccess {
                    object: Box::new(result),
                    field,
                };
            } else if self.has(Token::LeftBracket) {
                self.advance_tokens();
                let index = self.parse_expression()?;
                self.expect_token(Token::RightBracket)?;
                result = Expression::IndexAccess {
                    collection: Box::new(result),
                    index: Box::new(index),
                };
            } else {
                break;
            }
        }

        Ok(result)
    }

    fn parse_function_call(&mut self, function: Expression) -> Result<Expression> {
        self.expect_token(Token::LeftParen)?;

        let mut arguments = Vec::new();

        if !self.has(Token::RightParen) {
            loop {
                let arg = self.parse_expression()?;
                arguments.push(arg);

                if self.has(Token::Comma) {
                    self.advance_tokens();
                } else if self.has(Token::RightParen) {
                    break;
                } else {
                    return Err(self.unexpected(vec![Token::Comma, Token::RightParen]));
                }
            }
        }

        self.expect_token(Token::RightParen)?;

        Ok(Expression::FunctionCall {
            function: Box::new(function),
            arguments,
        })
    }

    fn parse_literal(&mut self) -> Result<Literal> {
        match &self.current.token {
            Token::IntegerLiteral(value) => {
                let lit = Literal::Integer(*value);
                self.advance_tokens();
                Ok(lit)
            }
            Token::FloatLiteral(value) => {
                let lit = Literal::Float(*value);
                self.advance_tokens();
                Ok(lit)
            }
            Token::StringLiteral(value) => {
                let lit = Literal::String(value.clone());
                self.advance_tokens();
                Ok(lit)
            }
            _ => todo!("TODO: Add parser error that can list expected AST nodes!"),
        }
    }

    fn expect_identifier(&mut self) -> Result<String> {
        match &self.current.token {
            Token::Identifier(name) => {
                let name = name.clone();
                self.advance_tokens();
                Ok(name)
            }
            _ => Err(self.unexpected(vec![Token::Identifier("".to_owned())])),
        }
    }

    fn current_token_as_binary_operator(&self) -> Option<BinaryOperator> {
        match self.current.token {
            Token::Plus => Some(ArithmeticOperator::Add.into()),
            Token::Minus => Some(ArithmeticOperator::Subtract.into()),
            Token::Asterisk => Some(ArithmeticOperator::Multiply.into()),
            Token::Slash => Some(ArithmeticOperator::Divide.into()),
            Token::DoubleEqual => Some(BinaryOperator::Equal),
            Token::NotEqual => Some(BinaryOperator::NotEqual),
            Token::GreaterThan => Some(BinaryOperator::GreaterThan),
            Token::LessThan => Some(BinaryOperator::LessThan),
            Token::GreaterEqual => Some(BinaryOperator::GreaterEqual),
            Token::LessEqual => Some(BinaryOperator::LessEqual),
            Token::Concat => Some(BinaryOperator::Concat),
            Token::Contains => Some(BinaryOperator::Contains),
            _ => None,
        }
    }

    fn current_token_as_unary_operator(&self) -> Option<UnaryOperator> {
        match self.current.token {
            Token::Minus => Some(UnaryOperator::Negate),
            Token::Bang => Some(UnaryOperator::Not),
            _ => None,
        }
    }

    fn current_token_as_assignment(&self) -> Option<AssignmentOperator> {
        match self.current.token {
            Token::Equal => Some(AssignmentOperator::Assign),
            _ => None,
        }
    }

    fn get_precedence(&self) -> Option<u8> {
        match self.current_token_as_binary_operator()? {
            BinaryOperator::Arithmetic(op) => match op {
                ArithmeticOperator::Multiply | ArithmeticOperator::Divide => Some(7),
                ArithmeticOperator::Add | ArithmeticOperator::Subtract => Some(6),
            },
            BinaryOperator::Concat => Some(5),
            BinaryOperator::GreaterThan
            | BinaryOperator::LessThan
            | BinaryOperator::GreaterEqual
            | BinaryOperator::LessEqual => Some(4),
            BinaryOperator::Equal | BinaryOperator::NotEqual => Some(3),
            BinaryOperator::Contains => Some(2),
        }
    }

    fn parse_record_declaration(&mut self) -> Result<RecordDeclaration> {
        self.expect_token(Token::Record)?;
        let name = self.expect_identifier()?;
        self.expect_token(Token::LeftBrace)?;

        let mut fields = Vec::new();

        while !self.has(Token::RightBrace) && !self.has(Token::Eof) {
            let field_name = self.expect_identifier()?;
            self.expect_token(Token::Colon)?;
            let type_annotation = self.parse_type_annotation()?;
            fields.push(Field {
                name: field_name,
                type_annotation,
            });
            // Optional: Handle commas or newlines between fields
        }

        self.expect_token(Token::RightBrace)?;

        Ok(RecordDeclaration { name, fields })
    }

    fn parse_resource_declaration(&mut self) -> Result<ResourceDeclaration> {
        self.expect_token(Token::Resource)?;
        let name = self.expect_identifier()?;
        self.expect_token(Token::LeftBrace)?;

        let mut fields = Vec::new();
        let mut constructor = None;
        let mut methods = Vec::new();

        self.skip_newline();
        while !self.has(Token::RightBrace) && !self.has(Token::Eof) {
            if self.has(Token::Constructor) {
                if constructor.is_some() {
                    todo!("TODO: Error handling, duplicate constructor");
                }
                constructor = Some(self.parse_constructor_declaration()?);
            } else if self.has(Token::Func) {
                let method = self.parse_function_declaration()?;
                methods.push(method);
            } else {
                let field_name = self.expect_identifier()?;
                self.expect_token(Token::Colon)?;
                let type_annotation = self.parse_type_annotation()?;
                fields.push(Field {
                    name: field_name,
                    type_annotation,
                });
                // Optional: Handle commas or newlines between fields
            }
            self.skip_newline();
        }

        self.expect_token(Token::RightBrace)?;

        Ok(ResourceDeclaration {
            name,
            fields,
            constructor,
            methods,
        })
    }

    fn parse_constructor_declaration(&mut self) -> Result<ConstructorDeclaration> {
        self.expect_token(Token::Constructor)?;

        self.expect_token(Token::LeftParen)?;

        let parameters = self.parse_parameters()?;

        self.expect_token(Token::RightParen)?;

        let body = self.parse_block()?;

        Ok(ConstructorDeclaration { parameters, body })
    }

    fn parse_enum_declaration(&mut self) -> Result<EnumDeclaration> {
        self.expect_token(Token::Enum)?;
        let name = self.expect_identifier()?;
        self.expect_token(Token::LeftBrace)?;

        let mut variants = Vec::new();

        while !self.has(Token::RightBrace) && !self.has(Token::Eof) {
            let variant_name = self.expect_identifier()?;
            variants.push(variant_name);
            // Optional: Handle commas or newlines between variants
        }

        self.expect_token(Token::RightBrace)?;

        Ok(EnumDeclaration { name, variants })
    }

    fn parse_variant_declaration(&mut self) -> Result<VariantDeclaration> {
        self.expect_token(Token::Variant)?;
        let name = self.expect_identifier()?;
        self.expect_token(Token::LeftBrace)?;

        let mut cases = Vec::new();

        while !self.has(Token::RightBrace) && !self.has(Token::Eof) {
            let case_name = self.expect_identifier()?;
            let associated_type = if self.has(Token::LeftParen) {
                self.advance_tokens();
                let type_annotation = self.parse_type_annotation()?;
                self.expect_token(Token::RightParen)?;
                Some(type_annotation)
            } else {
                None
            };
            cases.push(VariantCase {
                name: case_name,
                associated_type,
            });
            // Optional: Handle commas or newlines between cases
        }

        self.expect_token(Token::RightBrace)?;

        Ok(VariantDeclaration { name, cases })
    }

    fn parse_import_declaration(&mut self) -> Result<ImportDeclaration> {
        todo!("TODO: Implement import parsing")
    }

    fn parse_export_declaration(&mut self) -> Result<ExportDeclaration> {
        todo!("TODO: Implement export parsing")
    }

    fn parse_package_declaration(&mut self) -> Result<PackageDeclaration> {
        todo!("TODO: Implement package parsing")
    }

    fn parse_use_declaration(&mut self) -> Result<UseDeclaration> {
        todo!("TODO: Implement 'use' parsing")
    }
}

#[cfg(test)]
mod tests {
    use super::Result;
    use super::*;
    use crate::lexer::Lexer;

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
                            op: AssignmentOperator::Assign,
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
                            op: AssignmentOperator::Assign,
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

    // Tests for methods with TODOs

    #[test]
    fn test_parse_import_declaration() -> Result<()> {
        let input = r#"
        import math
        "#;

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let module = parser.parse_module()?;

        // TODO: Implement parse_import_declaration and update the test
        assert!(module.declarations.is_empty());

        Ok(())
    }

    #[test]
    fn test_parse_package_declaration() -> Result<()> {
        let input = r#"
        package test:all_features@1.0.0;
        "#;

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let module = parser.parse_module()?;

        // TODO: Implement parse_package_declaration and update the test
        assert!(module.declarations.is_empty());

        Ok(())
    }

    #[test]
    fn test_parse_use_declaration() -> Result<()> {
        let input = r#"
        use external:io::{print, println}
        "#;

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let module = parser.parse_module()?;

        // TODO: Implement parse_use_declaration and update the test
        assert!(module.declarations.is_empty());

        Ok(())
    }
}
