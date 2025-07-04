use crate::lexer::{Lexer, SpannedToken, Token};

use wtf_ast::*;
use wtf_error::{Error, ErrorKind};
use wtf_tokens::Span;

pub type Result<Ok> = std::result::Result<Ok, Error>;

enum BraceType {
    Paren,
    Bracket,
    Brace,
}

pub struct Parser {
    lexer: Lexer,
    current: SpannedToken,

    /// This disallows record literals in certain positions, e.g. if conditions
    allow_left_brace: bool,

    /// Parser errors that the parser recovered from. If this is not empty, do not proceed
    errors: Vec<Error>,
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
            errors: vec![],
        }
    }

    fn advance_tokens(&mut self) {
        self.current = self.lexer.next_token();
    }

    fn expect_token(&mut self, expected: Token) -> Result<Span> {
        let span = self.current.span;
        if self.has(expected.clone()) {
            self.advance_tokens();
            Ok(span)
        } else {
            Err(self.unexpected(vec![expected]))
        }
    }

    /// Produces an appropriate parser error for an unexpected token
    fn unexpected(&self, expected: Vec<Token>) -> Error {
        let found = self.current.token.clone();
        Error {
            span: self.current.span,
            kind: ErrorKind::UnexpectedToken { found, expected },
        }
    }

    fn skip_linebreaks(&mut self) {
        while self.current.token == Token::Newline || self.current.token == Token::EmptyLine {
            self.advance_tokens();
        }
    }

    fn skip_newline(&mut self) {
        while self.current.token == Token::Newline {
            self.advance_tokens();
        }
    }

    fn skip_semicolons(&mut self) {
        while self.current.token == Token::Semicolon {
            self.advance_tokens();
        }
    }

    /// Fast-forwards until the next matching Brace
    ///
    /// This function assumes that there is already a suitable opening brace
    fn recover(&mut self, paren_type: BraceType) {
        let mut nesting_depth = 1;
        let (open, close) = match paren_type {
            BraceType::Paren => (Token::LeftParen, Token::RightParen),
            BraceType::Bracket => (Token::LeftBracket, Token::RightBracket),
            BraceType::Brace => (Token::LeftBrace, Token::RightBrace),
        };

        while nesting_depth > 0 && self.current.token != Token::Eof {
            self.advance_tokens();

            if self.current.token == open {
                nesting_depth += 1
            } else if self.current.token == close {
                nesting_depth -= 1
            }
        }
    }

    pub fn errors(&self) -> &[Error] {
        &self.errors
    }

    pub fn chars(&self) -> &[char] {
        self.lexer.chars()
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
        self.skip_linebreaks();
        let package = if self.current.token == Token::Package {
            Some(self.parse_package_declaration()?)
        } else {
            None
        };

        self.skip_linebreaks();

        let mut uses = Vec::new();
        while self.current.token == Token::Use {
            uses.push(self.parse_use_declaration()?);
            self.skip_linebreaks();
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

            self.skip_linebreaks();
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
            Token::Overload => self.parse_overload_declaration().map(Declaration::Overload),
            Token::Record => self.parse_record_declaration().map(Declaration::Record),
            Token::Resource => self.parse_resource_declaration().map(Declaration::Resource),
            Token::Enum => self.parse_enum_declaration().map(Declaration::Enum),
            Token::Variant => self.parse_variant_declaration().map(Declaration::Variant),
            Token::Export => self.parse_export_declaration().map(Declaration::Export),
            Token::Test => self.parse_test_declaration().map(Declaration::Test),
            _t => Err(self.unexpected(vec![
                Token::Func,
                Token::Overload,
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
        let start = self.expect_token(Token::Func)?;

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

        let end = self.current.span;

        Ok(FunctionDeclaration {
            name,
            parameters,
            return_type,
            body,
            span: start.to(end),
        })
    }

    fn parse_overload_declaration(&mut self) -> Result<OverloadDeclaration> {
        let start = self.expect_token(Token::Overload)?;
        self.skip_linebreaks();

        let name = self.expect_identifier()?;
        self.skip_linebreaks();

        self.expect_token(Token::LeftBrace)?;
        self.skip_linebreaks();

        let mut overloads = Vec::new();
        while !self.has(Token::RightBrace) && !self.has(Token::Eof) {
            let name = match self.expect_identifier() {
                Ok(ident) => ident,
                Err(err) => {
                    self.errors.push(err);
                    self.recover(BraceType::Brace);
                    break;
                }
            };
            overloads.push(name);
            self.skip_linebreaks();
            while self.has(Token::Comma) {
                self.advance_tokens();
                self.skip_linebreaks();
            }
        }

        let end = self.expect_token(Token::RightBrace)?;

        Ok(OverloadDeclaration {
            name,
            overloads,
            span: start.to(end),
        })
    }

    fn parse_parameters(&mut self) -> Result<Vec<Parameter>> {
        let mut parameters = Vec::new();

        if self.has(Token::RightParen) {
            return Ok(parameters);
        }

        loop {
            let start = self.current.span;
            let name = self.expect_identifier()?;
            self.expect_token(Token::Colon)?;
            let type_annotation = self.parse_type_annotation()?;
            let end = type_annotation.span;
            parameters.push(Parameter {
                name,
                type_annotation,
                span: start.to(end),
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
            let start = self.current.span;
            self.advance_tokens();
            self.skip_linebreaks();

            let inner = self.parse_type_annotation()?;

            self.skip_linebreaks();
            let end = self.expect_token(Token::RightBracket)?;

            return Ok(TypeAnnotation::list(inner.into(), start.to(end)));
        }

        if let Token::Identifier(name) = self.current.token.clone() {
            let begin = self.current.span;
            let type_name = name.clone();
            self.advance_tokens();

            let mut ty = match type_name.as_str() {
                "list" => {
                    self.expect_token(Token::LessThan)?;
                    let inner_type = self.parse_type_annotation()?;
                    let end = self.expect_token(Token::GreaterThan)?;
                    TypeAnnotation::list(inner_type, begin.to(end))
                }
                "option" => {
                    self.expect_token(Token::LessThan)?;
                    let inner_type = self.parse_type_annotation()?;
                    let end = self.expect_token(Token::GreaterThan)?;
                    TypeAnnotation::option(inner_type, begin.to(end))
                }
                "result" => {
                    self.expect_token(Token::LessThan)?;
                    let ok = self.parse_type_annotation()?;
                    self.expect_token(Token::Comma)?;
                    let err = self.parse_type_annotation()?;
                    let end = self.expect_token(Token::GreaterThan)?;
                    TypeAnnotation::result(ok, err, begin.to(end))
                }
                _ => TypeAnnotation::simple(type_name, begin),
            };

            if allow_postfix {
                loop {
                    match self.current.token {
                        Token::QuestionMark => {
                            let end = self.current.span;
                            self.advance_tokens();
                            ty = TypeAnnotation::option(ty.into(), begin.to(end));
                        }
                        Token::Bang => {
                            self.advance_tokens();

                            let err = match self.current.token {
                                Token::Identifier(_) | Token::LeftBracket => {
                                    self.parse_type_annotation_with_postfix(false)?
                                }
                                _ => TypeAnnotation::simple(
                                    "error".to_owned(),
                                    Span {
                                        start: self.current.span.end,
                                        end: self.current.span.end,
                                    },
                                ),
                            };

                            let end = err.span;
                            ty = TypeAnnotation::result(ty.into(), err.into(), begin.to(end))
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
        let begin = self.expect_token(Token::LeftBrace)?;
        self.skip_newline();

        let mut statements = Vec::new();

        while !self.has(Token::RightBrace) && !self.has(Token::Eof) {
            if self.current.token == Token::EmptyLine {
                statements.push(Statement::EmptyLine);
                self.skip_linebreaks();

                continue;
            }

            let stmt = match self.parse_statement() {
                Ok(stmt) => stmt,
                Err(err) => {
                    self.errors.push(err);
                    self.recover(BraceType::Brace);

                    break;
                }
            };
            statements.push(stmt);
            self.skip_newline();
        }

        let end = self.expect_token(Token::RightBrace)?;

        Ok(Block {
            statements,
            span: begin.to(end),
        })
    }

    fn parse_statement(&mut self) -> Result<Statement> {
        let statement = match self.current.token {
            Token::Let | Token::Var => self
                .parse_variable_declaration()
                .map(Statement::VariableDeclaration)?,
            Token::If => self.parse_if_statement().map(Statement::IfStatement)?,
            Token::Return => self.parse_return_statement()?,
            Token::Throw => self.parse_throw_statement()?,
            Token::Break => {
                self.advance_tokens();
                Statement::BreakStatement(None) // No value for break
            }
            Token::Continue => {
                self.advance_tokens();
                Statement::ContinueStatement
            }
            Token::While => self
                .parse_while_statement()
                .map(Statement::WhileStatement)?,
            Token::For => self.parse_for_statement().map(Statement::ForStatement)?,
            Token::Match => self
                .parse_match_statement()
                .map(Statement::MatchStatement)?,
            Token::Assert => self.parse_assert_statement().map(Statement::Assertion)?,
            _ => {
                let expr = self.parse_expression()?;

                match self.current.token {
                    Token::Equal => {
                        // TODO: Check that target is a valid assignment target
                        self.advance_tokens();
                        let value = self.parse_expression()?;
                        Statement::Assignment {
                            target: expr,
                            value,
                        }
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

                        let right = self.parse_expression()?;

                        let target = expr.clone();
                        let operator = t.try_as_binary_op().expect("TODO: Error if there is another token than a binary operation as operator assignment");
                        let begin = expr.span;
                        let end = right.span;
                        let value = ExpressionKind::BinaryExpression {
                            left: expr.into(),
                            operator,
                            right: right.into(),
                        }
                        .spanned(begin.to(end));

                        Statement::Assignment { target, value }
                    }
                    _ => Statement::ExpressionStatement(expr),
                }
            }
        };
        self.skip_semicolons();

        Ok(statement)
    }

    fn parse_variable_declaration(&mut self) -> Result<VariableDeclaration> {
        let mutable = match self.current.token {
            Token::Var => true,
            Token::Let => false,
            _ => return Err(self.unexpected(vec![Token::Var, Token::Let])),
        };
        let begin = self.current.span;
        self.advance_tokens();

        let mut end = self.current.span;
        let name = self.expect_identifier()?;

        let type_annotation = if self.has(Token::Colon) {
            self.advance_tokens();
            let anno = self.parse_type_annotation()?;
            end = anno.span;
            Some(anno)
        } else {
            None
        };

        self.skip_linebreaks();
        let value = if self.current.token == Token::Equal {
            self.advance_tokens();
            self.skip_linebreaks();
            let expr = self.parse_expression()?;
            end = expr.span;
            Some(expr)
        } else {
            None
        };

        Ok(VariableDeclaration {
            mutable,
            name,
            type_annotation,
            value,
            span: begin.to(end),
        })
    }

    fn parse_if_statement(&mut self) -> Result<IfStatement> {
        let begin = self.expect_token(Token::If)?;

        self.allow_left_brace = false;
        let condition = self.parse_expression()?;
        self.allow_left_brace = true;

        let then_branch = self.parse_block()?;
        let mut end = then_branch.span;

        let else_branch = if self.has(Token::Else) {
            self.advance_tokens();
            let else_ = self.parse_block()?;
            end = else_.span;
            Some(else_)
        } else {
            None
        };

        Ok(IfStatement {
            condition,
            then_branch,
            else_branch,
            span: begin.to(end),
        })
    }

    fn parse_while_statement(&mut self) -> Result<WhileStatement> {
        let begin = self.expect_token(Token::While)?;

        let condition = self.parse_expression()?;

        let body = self.parse_block()?;
        let end = body.span;

        Ok(WhileStatement {
            condition,
            body,
            span: begin.to(end),
        })
    }

    fn parse_for_statement(&mut self) -> Result<ForStatement> {
        let begin = self.expect_token(Token::For)?;

        let variable = self.expect_identifier()?;

        self.expect_token(Token::Contains)?;

        self.allow_left_brace = false;
        let iterable = self.parse_expression()?;
        self.allow_left_brace = true;

        let body = self.parse_block()?;
        let end = body.span;

        Ok(ForStatement {
            variable,
            iterable,
            body,
            span: begin.to(end),
        })
    }

    fn parse_match_statement(&mut self) -> Result<MatchStatement> {
        let begin = self.expect_token(Token::Match)?;

        let expression = self.parse_expression()?;

        self.expect_token(Token::LeftBrace)?;

        let mut arms = Vec::new();

        // TODO: error recovery
        while !self.has(Token::RightBrace) && !self.has(Token::Eof) {
            let pattern = self.parse_pattern()?;
            let begin = pattern.span;
            self.expect_token(Token::Arrow)?;
            let body = if self.has(Token::LeftBrace) {
                self.parse_block()?
            } else {
                let expr = self.parse_expression()?;
                let span = expr.span;
                Block {
                    statements: vec![Statement::ExpressionStatement(expr)],
                    span,
                }
            };
            let end = body.span;
            arms.push(MatchArm {
                pattern,
                body,
                span: begin.to(end),
            });
            // Optional: Handle comma or semicolon between arms
        }

        let end = self.expect_token(Token::RightBrace)?;

        Ok(MatchStatement {
            expression,
            arms,
            span: begin.to(end),
        })
    }

    fn parse_assert_statement(&mut self) -> Result<AssertStatement> {
        let begin = self.expect_token(Token::Assert)?;

        let expression = self.parse_expression()?;
        let end = expression.span;

        Ok(AssertStatement {
            condition: expression,
            span: begin.to(end),
        })
    }

    fn parse_pattern(&mut self) -> Result<Pattern> {
        let begin = self.current.span;
        if let Some(name) = self.current_token_as_identifier() {
            self.advance_tokens();
            if self.has(Token::LeftParen) {
                self.advance_tokens();
                let sub_pattern = self.parse_pattern()?;
                let end = self.expect_token(Token::RightParen)?;
                Ok(PatternKind::VariantPattern {
                    variant_name: name.into(),
                    sub_pattern: Some(Box::new(sub_pattern)),
                }
                .spanned(begin.to(end)))
            } else {
                Ok(PatternKind::Identifier(name.into()).spanned(begin))
            }
        } else if let Ok(literal) = self.parse_literal() {
            let span = literal.span;
            Ok(PatternKind::Literal(literal).spanned(span))
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
            let begin = left.span;
            let end = right.span;
            left = ExpressionKind::BinaryExpression {
                left: Box::new(left),
                operator,
                right: Box::new(right),
            }
            .spanned(begin.to(end));
        }

        Ok(left)
    }

    fn parse_unary_expression(&mut self) -> Result<Expression> {
        let begin = self.current.span;
        if let Some(operator) = self.current_token_as_unary_operator() {
            self.advance_tokens();
            let operand = self.parse_unary_expression()?;
            let end = operand.span;
            Ok(ExpressionKind::UnaryExpression {
                operator,
                operand: Box::new(operand),
            }
            .spanned(begin.to(end)))
        } else {
            let mut expr = self.parse_primary_expression()?;

            // Handle postfix operators: '!' (yeet operator)
            if self.has(Token::Bang) {
                let end = self.current.span;
                self.advance_tokens();
                expr = ExpressionKind::YeetExpression {
                    expression: Box::new(expr),
                }
                .spanned(begin.to(end));
            }

            Ok(expr)
        }
    }

    fn parse_primary_expression(&mut self) -> Result<Expression> {
        let begin = self.current.span;
        match &self.current.token {
            Token::Identifier(name) => {
                let name = name.clone();

                self.advance_tokens();
                let expr = if self.current.token == Token::LeftBrace && self.allow_left_brace {
                    self.parse_record_lit(Some(name))?
                } else {
                    ExpressionKind::Identifier(name).spanned(begin)
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
                    self.skip_linebreaks();

                    if self.current.token == Token::RightBracket {
                        break;
                    }

                    elements.push(self.parse_expression()?);
                    self.skip_linebreaks();

                    if self.current.token != Token::Comma {
                        break;
                    }
                }

                let end = self.expect_token(Token::RightBracket)?;

                Ok(ExpressionKind::ListLiteral(elements).spanned(begin.to(end)))
            }
            Token::LeftBrace => {
                let record = self.parse_record_lit(None)?;

                self.parse_postfix_expression(record)
            }
            _ => Ok(ExpressionKind::Literal(self.parse_literal()?).spanned(begin)),
        }
    }

    /// This function assumes that the name was already parsed and is supplied if it exists
    fn parse_record_lit(&mut self, name: Option<String>) -> Result<Expression> {
        let begin = self.expect_token(Token::LeftBrace)?;

        let mut members = Vec::new();
        loop {
            self.skip_linebreaks();
            if self.current.token == Token::RightBrace {
                break;
            }

            let field = match self.parse_field_assignment() {
                Ok(field) => field,
                Err(err) => {
                    self.errors.push(err);
                    self.recover(BraceType::Brace);
                    break;
                }
            };

            members.push(field);

            self.skip_linebreaks();
            if self.current.token != Token::Comma {
                break;
            }
            self.advance_tokens();
        }
        let end = self.expect_token(Token::RightBrace)?;

        Ok(ExpressionKind::Record { name, members }.spanned(begin.to(end)))
    }

    fn parse_field_assignment(&mut self) -> Result<FieldAssignment> {
        let begin = self.current.span;
        let name = self
            .current_token_as_identifier()
            .ok_or_else(|| self.unexpected(vec![Token::Identifier("".to_owned())]))?;
        self.advance_tokens();

        let element = if self.current.token == Token::Colon {
            self.advance_tokens();
            self.parse_expression()?
        } else {
            ExpressionKind::Identifier(name.clone()).spanned(begin)
        };

        let end = element.span;

        Ok(FieldAssignment {
            name,
            element,
            span: begin.to(end),
        })
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
                let begin = self.current.span;
                result = if self.has(Token::LeftParen) {
                    let (arguments, end) = self.parse_arguments()?;
                    ExpressionKind::MethodCall {
                        receiver: result.into(),
                        method: field,
                        arguments,
                        safe,
                    }
                    .spanned(begin.to(end))
                } else {
                    ExpressionKind::FieldAccess {
                        object: Box::new(result),
                        field,
                        safe,
                    }
                    .spanned(begin)
                };
            } else if self.has(Token::LeftBracket) {
                self.advance_tokens();
                let index = self.parse_expression()?;
                let begin = result.span;
                let end = self.expect_token(Token::RightBracket)?;
                result = ExpressionKind::IndexAccess {
                    collection: Box::new(result),
                    index: Box::new(index),
                }
                .spanned(begin.to(end));
            } else {
                break;
            }
        }

        Ok(result)
    }

    fn parse_function_call(&mut self, function: Expression) -> Result<Expression> {
        let begin = function.span;
        let (arguments, end) = self.parse_arguments()?;

        Ok(ExpressionKind::FunctionCall {
            function: Box::new(function),
            arguments,
        }
        .spanned(begin.to(end)))
    }

    fn parse_arguments(&mut self) -> Result<(Vec<Expression>, Span)> {
        let begin = self.expect_token(Token::LeftParen)?;

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

        let end = self.expect_token(Token::RightParen)?;

        Ok((arguments, begin.to(end)))
    }

    fn parse_literal(&mut self) -> Result<Literal> {
        let span = self.current.span;
        let lit = match &self.current.token {
            Token::IntegerLiteral(value) => LiteralKind::Integer(*value),
            Token::FloatLiteral(value) => LiteralKind::Float(*value),
            Token::StringLiteral(value) => LiteralKind::String(value.clone()),
            Token::True => LiteralKind::Boolean(true),
            Token::False => LiteralKind::Boolean(false),
            Token::None => LiteralKind::None,
            _ => {
                return Err(self.unexpected(vec![
                    Token::IntegerLiteral(0),
                    Token::FloatLiteral(0.0),
                    Token::StringLiteral("".to_owned()),
                    Token::True,
                    Token::False,
                ]));
            }
        }
        .spanned(span);
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
                ArithmeticOperator::Multiply | ArithmeticOperator::Divide => Some(10),
                ArithmeticOperator::Add | ArithmeticOperator::Subtract => Some(9),
            },
            BinaryOperator::GreaterThan
            | BinaryOperator::LessThan
            | BinaryOperator::GreaterEqual
            | BinaryOperator::LessEqual => Some(8),
            BinaryOperator::Equal | BinaryOperator::NotEqual => Some(7),
            BinaryOperator::Contains => Some(6),
            BinaryOperator::NullCoalesce => Some(5),
            BinaryOperator::Logic(op) => match op {
                LogicOperator::And => Some(4),
                LogicOperator::Or => Some(3),
            },
        }
    }

    /// Parses a delimiter token (newline or comma) and returns it if one was parsed
    fn parse_delimiter(&mut self) -> Option<Token> {
        if self.current.token != Token::Newline && self.current.token != Token::Comma {
            return None;
        }
        let token = self.current.token.clone();
        self.advance_tokens();
        self.skip_linebreaks();

        Some(token)
    }

    fn parse_record_declaration(&mut self) -> Result<RecordDeclaration> {
        let begin = self.expect_token(Token::Record)?;
        let name = self.expect_identifier()?;
        self.expect_token(Token::LeftBrace)?;
        self.skip_linebreaks();

        let mut fields = Vec::new();

        while !self.has(Token::RightBrace) && !self.has(Token::Eof) {
            let field = match self.parse_field() {
                Ok(field) => field,
                Err(err) => {
                    self.errors.push(err);
                    self.recover(BraceType::Brace);
                    break;
                }
            };

            fields.push(field);

            // While the specification requires comma, semicolon or newline here, we parse this permissively, so that the formatter can correct a missing delimiter
            while self.has(Token::Comma)
                || self.has(Token::Newline)
                || self.has(Token::EmptyLine)
                || self.has(Token::Semicolon)
            {
                self.advance_tokens();
            }
        }

        self.skip_linebreaks();
        let end = self.expect_token(Token::RightBrace)?;

        Ok(RecordDeclaration {
            name,
            fields,
            span: begin.to(end),
        })
    }

    fn parse_field(&mut self) -> Result<Field> {
        let begin = self.current.span;
        let field_name = self.expect_identifier()?;
        self.expect_token(Token::Colon)?;
        let type_annotation = self.parse_type_annotation()?;
        let end = type_annotation.span;

        Ok(Field {
            name: field_name,
            type_annotation,
            span: begin.to(end),
        })
    }

    fn parse_resource_declaration(&mut self) -> Result<ResourceDeclaration> {
        let begin = self.expect_token(Token::Resource)?;
        let name = self.expect_identifier()?;
        self.expect_token(Token::LeftBrace)?;

        let mut fields = Vec::new();
        let mut constructor = None;
        let mut methods = Vec::new();

        self.skip_linebreaks();
        // TODO: error recovery
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
                let begin = self.current.span;
                let field_name = self.expect_identifier()?;
                self.expect_token(Token::Colon)?;
                let type_annotation = self.parse_type_annotation()?;
                let end = type_annotation.span;
                fields.push(Field {
                    name: field_name,
                    type_annotation,
                    span: begin.to(end),
                });
                // Optional: Handle commas or newlines between fields
            }
            self.skip_linebreaks();
        }

        let end = self.expect_token(Token::RightBrace)?;

        Ok(ResourceDeclaration {
            name,
            fields,
            constructor,
            methods,
            span: begin.to(end),
        })
    }

    fn parse_constructor_declaration(&mut self) -> Result<ConstructorDeclaration> {
        let begin = self.expect_token(Token::Constructor)?;

        self.expect_token(Token::LeftParen)?;

        let parameters = self.parse_parameters()?;

        self.expect_token(Token::RightParen)?;

        let body = self.parse_block()?;
        let end = body.span;

        Ok(ConstructorDeclaration {
            parameters,
            body,
            span: begin.to(end),
        })
    }

    fn parse_enum_declaration(&mut self) -> Result<EnumDeclaration> {
        let begin = self.expect_token(Token::Enum)?;
        let name = self.expect_identifier()?;
        self.expect_token(Token::LeftBrace)?;
        self.skip_linebreaks();

        let mut variants = Vec::new();

        // TODO: error recovery
        while !self.has(Token::RightBrace) && !self.has(Token::Eof) {
            let variant_name = self.expect_identifier()?;
            variants.push(variant_name);

            if self.parse_delimiter().is_none() {
                break;
            }
            self.skip_linebreaks();
        }

        self.skip_linebreaks();
        let end = self.expect_token(Token::RightBrace)?;

        Ok(EnumDeclaration {
            name,
            cases: variants,
            span: begin.to(end),
        })
    }

    fn parse_variant_declaration(&mut self) -> Result<VariantDeclaration> {
        let begin = self.expect_token(Token::Variant)?;
        let name = self.expect_identifier()?;
        self.expect_token(Token::LeftBrace)?;
        self.skip_linebreaks();

        let mut cases = Vec::new();

        // TODO: error recovery
        while !self.has(Token::RightBrace) && !self.has(Token::Eof) {
            let begin = self.current.span;
            let mut end = begin;
            let case_name = self.expect_identifier()?;
            let mut associated_types = Vec::new();

            if self.has(Token::LeftParen) {
                self.advance_tokens();
                self.skip_linebreaks();

                while !self.has(Token::RightParen) && !self.has(Token::Eof) {
                    let begin = self.current.span;
                    let field_name = self.expect_identifier()?;
                    self.expect_token(Token::Colon)?;
                    let type_annotation = self.parse_type_annotation()?;
                    let end = type_annotation.span;
                    associated_types.push(Field {
                        name: field_name,
                        type_annotation,
                        span: begin.to(end),
                    });

                    if self.parse_delimiter().is_none() {
                        break;
                    }
                    self.skip_linebreaks();
                }
                end = self.expect_token(Token::RightParen)?;
            }
            cases.push(VariantCase {
                name: case_name,
                associated_types,
                span: begin.to(end),
            });

            if self.parse_delimiter().is_none() {
                break;
            }
        }

        self.skip_linebreaks();
        let end = self.expect_token(Token::RightBrace)?;

        Ok(VariantDeclaration {
            name,
            cases,
            span: begin.to(end),
        })
    }

    fn parse_export_declaration(&mut self) -> Result<ExportDeclaration> {
        let begin = self.expect_token(Token::Export)?;
        let item = self.parse_declaration()?;
        let end = item.span();

        Ok(ExportDeclaration {
            item: item.into(),
            span: begin.to(end),
        })
    }

    fn parse_test_declaration(&mut self) -> Result<TestDeclaration> {
        let begin = self.expect_token(Token::Test)?;
        self.skip_linebreaks();
        let name = match self.current.token {
            Token::StringLiteral(ref name) => {
                let name = name.to_owned();
                self.skip_linebreaks();
                self.advance_tokens();
                Some(name)
            }
            Token::LeftBrace => None,
            _ => panic!("Test names should be a string"),
        };
        self.skip_linebreaks();
        let body = self.parse_block()?;
        let end = body.span;

        Ok(TestDeclaration {
            name,
            body,
            span: begin.to(end),
        })
    }

    fn parse_package_declaration(&mut self) -> Result<PackageDeclaration> {
        let begin = self.expect_token(Token::Package)?;
        let path = self.parse_path()?;
        let mut end = path.span;

        let version = if self.current.token == Token::At {
            self.advance_tokens();
            let version = self.parse_version()?;
            end = version.span;
            Some(version)
        } else {
            None
        };

        self.skip_semicolons();

        Ok(PackageDeclaration {
            path,
            version,
            span: begin.to(end),
        })
    }

    fn parse_use_declaration(&mut self) -> Result<UseDeclaration> {
        let begin = self.expect_token(Token::Use)?;
        let module_path = self.parse_path()?;

        self.expect_token(Token::Slash)?;
        let interface = self.expect_identifier()?;

        self.expect_token(Token::Dot)?;
        self.expect_token(Token::LeftBrace)?;
        let mut types = Vec::new();
        // TODO: error recovery
        while self.current.token != Token::RightBrace && types.is_empty()
            || self.expect_token(Token::Comma).is_ok()
        {
            let ty = self.expect_identifier()?;
            types.push(ty);
        }

        let end = self.expect_token(Token::RightBrace)?;

        Ok(UseDeclaration {
            module_path,
            interface,
            types,
            span: begin.to(end),
        })
    }

    fn parse_path(&mut self) -> Result<ModulePath> {
        let begin = self.current.span;
        let owner = self.expect_identifier()?;
        self.expect_token(Token::Colon)?;
        let end = self.current.span;
        let package = self.expect_identifier()?;

        Ok(ModulePath {
            owner,
            package,
            span: begin.to(end),
        })
    }

    fn parse_version(&mut self) -> Result<Version> {
        let span = self.current.span;
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
            span,
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
            Token::And => Some(LogicOperator::And.into()),
            Token::Or => Some(LogicOperator::Or.into()),
            _ => None,
        }
    }
}
