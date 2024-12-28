use std::fmt::Debug;

use crate::lexer::{Lexer, SpannedToken, Token};

use wtf_ast::*;

#[derive(Clone, Debug)]
pub struct ParserError {
    found: SpannedToken,
    expected: Vec<Token>,
}

pub type Result<Ok> = std::result::Result<Ok, ParserError>;

pub struct Parser {
    lexer: Lexer,
    current: SpannedToken,

    // This disallows record literals in certain positions, e.g. if
    allow_left_brace: bool,
}

impl Parser {
    pub fn new(source: &str) -> Self {
        let lexer = Lexer::new(source);
        Self::with_lexer(lexer)
    }

    pub fn with_lexer(mut lexer: Lexer) -> Self {
        let current = lexer.next_token();
        Parser {
            lexer,
            current,
            allow_left_brace: true,
        }
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
        self.skip_newline();
        let package = if self.current.token == Token::Package {
            Some(self.parse_package_declaration()?)
        } else {
            None
        };

        self.skip_newline();

        let mut uses = Vec::new();
        while self.current.token == Token::Use {
            uses.push(self.parse_use_declaration()?);
            self.skip_newline();
        }

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

        Ok(Module {
            package,
            declarations,
            uses,
        })
    }

    fn parse_declaration(&mut self) -> Result<Declaration> {
        match &self.current.token {
            Token::Func => self.parse_function_declaration().map(Declaration::Function),
            Token::Record => self.parse_record_declaration().map(Declaration::Record),
            Token::Resource => self.parse_resource_declaration().map(Declaration::Resource),
            Token::Enum => self.parse_enum_declaration().map(Declaration::Enum),
            Token::Variant => self.parse_variant_declaration().map(Declaration::Variant),
            Token::Export => self.parse_export_declaration().map(Declaration::Export),
            Token::Test => self.parse_test_declaration().map(Declaration::Test),
            _t => Err(self.unexpected(vec![
                Token::Func,
                Token::Record,
                Token::Resource,
                Token::Enum,
                Token::Variant,
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
        self.parse_type_annotation_with_postfix(true)
    }

    /// Parses a type annotation, e.g. of a record field or a variable declaration
    ///
    /// Arguments:
    /// - allow_postfix: Whether the type in this position allows for postfix '?' or '!'. In some positions (e.g. error types, this should be false)
    fn parse_type_annotation_with_postfix(
        &mut self,
        allow_postfix: bool,
    ) -> Result<TypeAnnotation> {
        // Handle syntactic sugar for list types ('[T]') first
        if self.current.token == Token::LeftBracket {
            self.advance_tokens();
            self.skip_newline();

            let inner = self.parse_type_annotation()?;

            self.skip_newline();
            self.expect_token(Token::RightBracket)?;

            return Ok(TypeAnnotation::List(inner.into()));
        }

        if let Token::Identifier(name) = self.current.token.clone() {
            let type_name = name.clone();
            self.advance_tokens();

            let mut ty = match type_name.as_str() {
                "list" => {
                    self.expect_token(Token::LessThan)?;
                    let inner_type = self.parse_type_annotation()?;
                    self.expect_token(Token::GreaterThan)?;
                    TypeAnnotation::List(Box::new(inner_type))
                }
                "option" => {
                    self.expect_token(Token::LessThan)?;
                    let inner_type = self.parse_type_annotation()?;
                    self.expect_token(Token::GreaterThan)?;
                    TypeAnnotation::Option(Box::new(inner_type))
                }
                "result" => {
                    self.expect_token(Token::LessThan)?;
                    let ok = self.parse_type_annotation()?;
                    self.expect_token(Token::Comma)?;
                    let err = self.parse_type_annotation()?;
                    self.expect_token(Token::GreaterThan)?;
                    TypeAnnotation::Result {
                        ok: Box::new(ok),
                        err: Box::new(err),
                    }
                }
                _ => TypeAnnotation::Simple(type_name),
            };

            if allow_postfix {
                loop {
                    match self.current.token {
                        Token::QuestionMark => {
                            self.advance_tokens();
                            ty = TypeAnnotation::Option(ty.into());
                        }
                        Token::Bang => {
                            self.advance_tokens();

                            let err = match self.current.token {
                                Token::Identifier(_) | Token::LeftBracket => {
                                    self.parse_type_annotation_with_postfix(false)?
                                }
                                _ => TypeAnnotation::Simple("error".to_owned()),
                            };

                            ty = TypeAnnotation::Result {
                                ok: ty.into(),
                                err: err.into(),
                            }
                        }
                        // TODO: Allow grouping with parens? would clash with tuple type annotations though
                        _ => break,
                    }
                }
            }

            Ok(ty)
        } else {
            // TODO: Add expected ast nodes
            Err(self.unexpected(vec![]))
        }
    }

    fn parse_block(&mut self) -> Result<Block> {
        self.expect_token(Token::LeftBrace)?;
        self.skip_newline();

        let mut statements = Vec::new();

        while !self.has(Token::RightBrace) && !self.has(Token::Eof) {
            let stmt = self.parse_statement()?;
            statements.push(stmt);
            self.skip_newline();
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
            Token::Assert => self.parse_assert_statement().map(Statement::Assertion),
            _ => {
                let expr = self.parse_expression()?;

                match self.current.token {
                    Token::Equal => {
                        // TODO: Check that target is a valid assignment target
                        self.advance_tokens();
                        let value = self.parse_expression()?;
                        Ok(Statement::Assignment {
                            target: expr,
                            value,
                        })
                    }
                    ref t @ (Token::Plus
                    | Token::Minus
                    | Token::Slash
                    | Token::Asterisk
                    | Token::QuestionMark) => {
                        // We can safely expect an = here as a binary operator would have been consumed by parse_expression already
                        let t = t.clone();
                        self.advance_tokens();
                        self.expect_token(Token::Equal)?;

                        let right = self.parse_expression()?.into();

                        let target = expr.clone();
                        let operator = t.try_as_binary_op().expect("TODO: Error if there is another token than a binary operation as operator assignment");
                        let value = Expression::BinaryExpression {
                            left: expr.into(),
                            operator,
                            right,
                        };

                        Ok(Statement::Assignment { target, value })
                    }
                    _ => Ok(Statement::ExpressionStatement(expr)),
                }
            }
        }
    }

    fn parse_variable_declaration(&mut self) -> Result<VariableDeclaration> {
        let mutable = match self.current.token {
            Token::Var => true,
            Token::Let => false,
            _ => return Err(self.unexpected(vec![Token::Var, Token::Let])),
        };
        self.advance_tokens();

        let name = self.expect_identifier()?;

        let type_annotation = if self.has(Token::Colon) {
            self.advance_tokens();
            Some(self.parse_type_annotation()?)
        } else {
            None
        };

        self.skip_newline();
        let value = if self.current.token == Token::Equal {
            self.advance_tokens();
            self.skip_newline();
            Some(self.parse_expression()?)
        } else {
            None
        };

        Ok(VariableDeclaration {
            mutable,
            name,
            type_annotation,
            value,
        })
    }

    fn parse_if_statement(&mut self) -> Result<IfStatement> {
        self.expect_token(Token::If)?;

        self.allow_left_brace = false;
        let condition = self.parse_expression()?;
        self.allow_left_brace = true;

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

    fn parse_assert_statement(&mut self) -> Result<AssertStatement> {
        todo!("Parse assertion")
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
            // The binary operator belongs to an assignment in this case
            if self.lexer.peek().token == Token::Equal {
                break;
            }
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
            Token::Identifier(name) => {
                let name = name.clone();

                self.advance_tokens();
                let expr = if self.current.token == Token::LeftBrace && self.allow_left_brace {
                    self.parse_record_lit(Some(name))?
                } else {
                    Expression::Identifier(name)
                };

                self.parse_postfix_expression(expr)
            }
            Token::LeftParen => {
                self.advance_tokens();
                let expr = self.parse_expression()?;
                self.expect_token(Token::RightParen)?;

                Ok(expr)
            }
            Token::LeftBracket => {
                let mut elements = Vec::new();
                loop {
                    self.advance_tokens();
                    self.skip_newline();

                    if self.current.token == Token::RightBracket {
                        break;
                    }

                    elements.push(self.parse_expression()?);
                    self.skip_newline();

                    if self.current.token != Token::Comma {
                        break;
                    }
                }

                self.expect_token(Token::RightBracket)?;

                Ok(Expression::ListLiteral(elements))
            }
            Token::LeftBrace => {
                let record = self.parse_record_lit(None)?;

                self.parse_postfix_expression(record)
            }
            _ => Ok(Expression::Literal(self.parse_literal()?)),
        }
    }

    /// This function assumes that the name was already parsed and is supplied if it exists
    fn parse_record_lit(&mut self, name: Option<String>) -> Result<Expression> {
        self.expect_token(Token::LeftBrace)?;

        let mut members = Vec::new();
        loop {
            self.skip_newline();
            if self.current.token == Token::RightBrace {
                break;
            }

            let name = self
                .current_token_as_identifier()
                .ok_or_else(|| self.unexpected(vec![Token::Identifier("".to_owned())]))?;
            self.advance_tokens();

            let element = if self.current.token == Token::Colon {
                self.advance_tokens();
                self.parse_expression()?
            } else {
                Expression::Identifier(name.clone())
            };

            members.push(FieldAssignment { name, element });

            self.skip_newline();
            if self.current.token != Token::Comma {
                break;
            }
            self.advance_tokens();
        }
        self.expect_token(Token::RightBrace)?;

        Ok(Expression::Record { name, members })
    }

    fn parse_postfix_expression(&mut self, expr: Expression) -> Result<Expression> {
        let mut result = expr;

        loop {
            if self.has(Token::LeftParen) {
                result = self.parse_function_call(result)?;
            } else if self.has(Token::Dot) || self.has(Token::SafeCall) {
                let safe = self.has(Token::SafeCall);
                self.advance_tokens();
                let field = self.expect_identifier()?;
                // TODO: Parse parameter
                result = if self.has(Token::LeftParen) {
                    let arguments = self.parse_arguments()?;
                    Expression::MethodCall {
                        receiver: result.into(),
                        method: field,
                        arguments,
                        safe,
                    }
                } else {
                    Expression::FieldAccess {
                        object: Box::new(result),
                        field,
                        safe,
                    }
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
        let arguments = self.parse_arguments()?;

        Ok(Expression::FunctionCall {
            function: Box::new(function),
            arguments,
        })
    }

    fn parse_arguments(&mut self) -> Result<Vec<Expression>> {
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

        Ok(arguments)
    }

    fn parse_literal(&mut self) -> Result<Literal> {
        let lit = match &self.current.token {
            Token::IntegerLiteral(value) => Literal::Integer(*value),
            Token::FloatLiteral(value) => Literal::Float(*value),
            Token::StringLiteral(value) => Literal::String(value.clone()),
            Token::True => Literal::Boolean(true),
            Token::False => Literal::Boolean(false),
            Token::None => Literal::None,
            _ => todo!("TODO: Add parser error that can list expected AST nodes!"),
        };
        self.advance_tokens();

        Ok(lit)
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
        self.current.token.try_as_binary_op()
    }

    fn current_token_as_unary_operator(&self) -> Option<UnaryOperator> {
        match self.current.token {
            Token::Minus => Some(UnaryOperator::Negate),
            Token::Bang => Some(UnaryOperator::Not),
            _ => None,
        }
    }

    fn get_precedence(&self) -> Option<u8> {
        match self.current_token_as_binary_operator()? {
            BinaryOperator::Arithmetic(op) => match op {
                ArithmeticOperator::Multiply | ArithmeticOperator::Divide => Some(7),
                ArithmeticOperator::Add | ArithmeticOperator::Subtract => Some(6),
            },
            BinaryOperator::GreaterThan
            | BinaryOperator::LessThan
            | BinaryOperator::GreaterEqual
            | BinaryOperator::LessEqual => Some(4),
            BinaryOperator::Equal | BinaryOperator::NotEqual => Some(3),
            BinaryOperator::Contains => Some(2),
            BinaryOperator::NullCoalesce => Some(1),
        }
    }

    /// Parses a delimiter token (newline or comma) and returns it if one was parsed
    fn parse_delimiter(&mut self) -> Option<Token> {
        if self.current.token != Token::Newline && self.current.token != Token::Comma {
            return None;
        }
        let token = self.current.token.clone();
        self.advance_tokens();
        self.skip_newline();

        Some(token)
    }

    fn parse_record_declaration(&mut self) -> Result<RecordDeclaration> {
        self.expect_token(Token::Record)?;
        let name = self.expect_identifier()?;
        self.expect_token(Token::LeftBrace)?;
        self.skip_newline();

        let mut fields = Vec::new();

        while !self.has(Token::RightBrace) && !self.has(Token::Eof) {
            let field_name = self.expect_identifier()?;
            self.expect_token(Token::Colon)?;
            let type_annotation = self.parse_type_annotation()?;
            fields.push(Field {
                name: field_name,
                type_annotation,
            });

            if self.parse_delimiter().is_none() {
                break;
            }
        }

        self.skip_newline();
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
        self.skip_newline();

        let mut variants = Vec::new();

        while !self.has(Token::RightBrace) && !self.has(Token::Eof) {
            let variant_name = self.expect_identifier()?;
            variants.push(variant_name);

            if self.parse_delimiter().is_none() {
                break;
            }
            self.skip_newline();
        }

        self.skip_newline();
        self.expect_token(Token::RightBrace)?;

        Ok(EnumDeclaration {
            name,
            cases: variants,
        })
    }

    fn parse_variant_declaration(&mut self) -> Result<VariantDeclaration> {
        self.expect_token(Token::Variant)?;
        let name = self.expect_identifier()?;
        self.expect_token(Token::LeftBrace)?;
        self.skip_newline();

        let mut cases = Vec::new();

        while !self.has(Token::RightBrace) && !self.has(Token::Eof) {
            let case_name = self.expect_identifier()?;
            let mut associated_types = Vec::new();

            if self.has(Token::LeftParen) {
                self.advance_tokens();
                self.skip_newline();

                while !self.has(Token::RightParen) && !self.has(Token::Eof) {
                    let field_name = self.expect_identifier()?;
                    self.expect_token(Token::Colon)?;
                    let type_annotation = self.parse_type_annotation()?;
                    associated_types.push(Field {
                        name: field_name,
                        type_annotation,
                    });

                    if self.parse_delimiter().is_none() {
                        break;
                    }
                    self.skip_newline();
                }
                self.expect_token(Token::RightParen)?;
            }
            cases.push(VariantCase {
                name: case_name,
                associated_types,
            });

            if self.parse_delimiter().is_none() {
                break;
            }
        }

        self.skip_newline();
        self.expect_token(Token::RightBrace)?;

        Ok(VariantDeclaration { name, cases })
    }

    fn parse_export_declaration(&mut self) -> Result<ExportDeclaration> {
        self.expect_token(Token::Export)?;
        let item = self.parse_declaration()?.into();

        Ok(ExportDeclaration { item })
    }

    fn parse_test_declaration(&mut self) -> Result<TestDeclaration> {
        self.expect_token(Token::Test)?;
        self.skip_newline();
        let name = match self.current.token {
            Token::StringLiteral(ref name) => {
                let name = name.to_owned();
                self.skip_newline();
                self.advance_tokens();
                name
            }
            Token::LeftBrace => String::new(),
            _ => panic!("Test names should be a string"),
        };
        self.skip_newline();
        let body = self.parse_block()?;

        Ok(TestDeclaration { name, body })
    }

    fn parse_package_declaration(&mut self) -> Result<PackageDeclaration> {
        self.expect_token(Token::Package)?;
        let path = self.parse_path()?;

        let version = if self.current.token == Token::At {
            self.advance_tokens();
            Some(self.parse_version()?)
        } else {
            None
        };

        Ok(PackageDeclaration { path, version })
    }

    fn parse_use_declaration(&mut self) -> Result<UseDeclaration> {
        self.expect_token(Token::Use)?;
        let module_path = self.parse_path()?;

        self.expect_token(Token::Slash)?;
        let interface = self.expect_identifier()?;

        self.expect_token(Token::Dot)?;
        self.expect_token(Token::LeftBrace)?;
        let mut types = Vec::new();
        while self.current.token != Token::RightBrace && types.is_empty()
            || self.expect_token(Token::Comma).is_ok()
        {
            let ty = self.expect_identifier()?;
            types.push(ty);
        }

        self.expect_token(Token::RightBrace)?;

        Ok(UseDeclaration {
            module_path,
            interface,
            types,
        })
    }

    fn parse_path(&mut self) -> Result<ModulePath> {
        let owner = self.expect_identifier()?;
        self.expect_token(Token::Colon)?;
        let package = self.expect_identifier()?;

        Ok(ModulePath { owner, package })
    }

    fn parse_version(&mut self) -> Result<Version> {
        let version_string = match &self.current.token {
            Token::VersionLiteral(s) => s.to_owned(),
            t @ Token::FloatLiteral(_) => {
                if let Token::VersionLiteral(s) = t.try_as_version_literal().into_owned() {
                    s
                } else {
                    return Err(self.unexpected(vec![Token::VersionLiteral("".to_owned())]));
                }
            }
            _ => {
                return Err(self.unexpected(vec![Token::VersionLiteral("".to_owned())]));
            }
        };

        let mut parts = version_string.split(".");
        let major = parts
            .next()
            .expect("TODO: parser error on invalid version string");
        let minor = parts.next();
        let patch = parts.next();
        let extras = parts.next();

        self.advance_tokens();

        Ok(Version {
            major: major
                .parse()
                .expect("TODO: Parser error on invalid major version"),
            minor: minor
                .map(|m| {
                    m.parse()
                        .expect("TODO: Parser error on invalid minor version")
                })
                .unwrap_or_default(),
            patch: patch
                .map(|p| {
                    p.parse()
                        .expect("TODO: Parser error on invalid patch version")
                })
                .unwrap_or_default(),
            extras: extras.map(str::to_owned),
        })
    }
}

trait TokenExt {
    fn try_as_binary_op(&self) -> Option<BinaryOperator>;
}

impl TokenExt for Token {
    fn try_as_binary_op(&self) -> Option<BinaryOperator> {
        match self {
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
            Token::Contains => Some(BinaryOperator::Contains),
            Token::QuestionMark => Some(BinaryOperator::NullCoalesce),
            _ => None,
        }
    }
}
