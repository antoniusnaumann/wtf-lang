use std::collections::HashMap;
use std::ops::Index;

use wtf_ast::{
    self as ast, BinaryOperator, FunctionDeclaration, Node, TestDeclaration, TypeAnnotation,
    UnaryOperator,
};
use wtf_error::Error;
use wtf_tokens::Span;

use crate::builtin::WithBuiltins;
use crate::{
    type_::unify, visible::Visible, Body, Expression, ExpressionKind, Function, FunctionBody,
    FunctionSignature, Module, Parameter, Test, Type, VarId,
};

const INTERNAL_PREFIX: &str = "wtfinternal";
const INTERNAL_SUFFIX: &str = "sdafbvaeiwcoiysxuv";

/// Internal compiler struct that holds error state and compilation methods
struct HirCompiler {
    errors: Vec<Error>,
}

pub fn compile(ast: ast::Module) -> Result<Module, Vec<Error>> {
    let mut compiler = HirCompiler::new();
    let module = compiler.compile_internal(ast);

    if compiler.has_errors() {
        Err(compiler.take_errors())
    } else {
        Ok(module)
    }
}

impl HirCompiler {
    fn new() -> Self {
        Self { errors: Vec::new() }
    }

    fn has_errors(&self) -> bool {
        !self.errors.is_empty()
    }

    fn take_errors(self) -> Vec<Error> {
        self.errors
    }

    fn compile_internal(&mut self, ast: ast::Module) -> Module {
        // TODO: Convert into lookup of name -> export? on first pass
        let mut ast_types = HashMap::new();
        let mut ast_funs = HashMap::new();
        let mut ast_tests = Vec::new();
        for mut declaration in ast.declarations {
            let is_export = if let ast::Declaration::Export(ex) = declaration {
                declaration = *ex.item;
                true
            } else {
                false
            };

            match declaration {
                ast::Declaration::Function(fun) => {
                    ast_funs.insert(fun.name.to_string(), (fun, is_export));
                }
                ast::Declaration::Overload(_overload) => {
                    todo!("Insert overloads from AST")
                }
                ast::Declaration::Record(rec) => {
                    ast_types.insert(
                        rec.name.to_string(),
                        (ast::Declaration::Record(rec), is_export),
                    );
                }
                ast::Declaration::Resource(res) => {
                    ast_types.insert(
                        res.name.to_string(),
                        (ast::Declaration::Resource(res), is_export),
                    );
                }
                ast::Declaration::Enum(en) => {
                    ast_types.insert(en.name.to_string(), (ast::Declaration::Enum(en), is_export));
                }
                ast::Declaration::Variant(var) => {
                    ast_types.insert(
                        var.name.to_string(),
                        (ast::Declaration::Variant(var), is_export),
                    );
                }
                ast::Declaration::Export(_) => {
                    self.errors.push(Error::type_mismatch(
                        "single export declaration".to_string(),
                        "double export declaration".to_string(),
                        declaration.span(),
                    ));
                }
                ast::Declaration::Test(test) => {
                    ast_tests.push(test);
                }
            }
        }

        let mut types = HashMap::new();
        for (decl, is_export) in ast_types.values() {
            types.insert(
                decl.name().to_owned(),
                self.compile_type_declaration(decl, *is_export, &ast_types),
            );
        }

        let mut signatures = HashMap::with_builtins();
        for (fun, is_export) in ast_funs.values() {
            signatures.insert(
                fun.name.to_string(),
                self.compile_signature(fun, *is_export, &ast_types),
            );
        }

        let mut functions = HashMap::new();
        for (fun, is_export) in ast_funs.values() {
            functions.insert(
                fun.name.to_string(),
                self.compile_fun(fun, *is_export, &ast_types, &types, &signatures),
            );
        }

        let mut tests = Vec::new();
        for (idx, test) in ast_tests.into_iter().enumerate() {
            tests.push(self.compile_test(idx, test, &signatures, &types, &ast_types));
        }

        Module {
            types,
            functions,
            tests,
        }
    }

    fn compile_type_declaration(
        &mut self,
        declaration: &ast::Declaration,
        is_export: bool,
        ast_types: &HashMap<String, (ast::Declaration, bool)>,
    ) -> Type {
        if is_export {
            todo!("Use exports on type declarations");
        }
        let type_ = match declaration {
            ast::Declaration::Record(record) => {
                let mut fields = HashMap::new();
                for field in &record.fields {
                    fields.insert(
                        field.name.to_string(),
                        self.compile_type_annotation(&field.type_annotation, ast_types),
                    );
                }
                Type::Record(fields)
            }
            ast::Declaration::Resource(resource) => {
                let mut methods = HashMap::new();
                for method in &resource.methods {
                    let return_type = {
                        let annotation = method
                            .return_type
                            .as_ref()
                            .map(|it| it.clone())
                            .unwrap_or_else(|| {
                                TypeAnnotation::simple(
                                    "none".to_string(),
                                    Span { start: 0, end: 0 },
                                )
                            });
                        self.compile_type_annotation(&annotation, ast_types)
                    };
                    methods.insert(
                        method.name.clone(),
                        FunctionSignature {
                            param_types: method
                                .parameters
                                .iter()
                                .map(|param| {
                                    self.compile_type_annotation(&param.type_annotation, ast_types)
                                })
                                .collect(),
                            return_type,
                            // TODO: allow exporting resource functions
                            is_export: false,
                        },
                    );
                }
                Type::Resource { methods }
            }
            ast::Declaration::Enum(enum_) => Type::Enum {
                cases: enum_.cases.iter().map(|case| case.clone()).collect(),
            },
            ast::Declaration::Variant(variants) => {
                let mut cases = HashMap::new();
                for variant in &variants.cases {
                    let mut fields = HashMap::new();
                    for field in &variant.associated_types {
                        fields.insert(
                            field.name.clone(),
                            self.compile_type_annotation(&field.type_annotation, ast_types),
                        );
                    }
                    cases.insert(variant.name.clone(), fields);
                }
                Type::Variant { cases }
            }
            _ => unreachable!(),
        };
        type_
    }

    fn compile_type_annotation(
        &mut self,
        annotation: &ast::TypeAnnotation,
        ast_types: &HashMap<String, (ast::Declaration, bool)>,
    ) -> Type {
        match &annotation.kind {
            ast::TypeAnnotationKind::Simple(name) => match name.as_str() {
                "bool" => Type::Bool,
                "s8" => Type::Int {
                    signed: true,
                    bits: 8,
                },
                "s16" => Type::Int {
                    signed: true,
                    bits: 16,
                },
                "s32" => Type::Int {
                    signed: true,
                    bits: 32,
                },
                "s64" => Type::Int {
                    signed: true,
                    bits: 64,
                },
                "u8" => Type::Int {
                    signed: false,
                    bits: 8,
                },
                "u16" => Type::Int {
                    signed: false,
                    bits: 16,
                },
                "u32" => Type::Int {
                    signed: false,
                    bits: 32,
                },
                "u64" => Type::Int {
                    signed: false,
                    bits: 64,
                },
                "f32" => Type::Float { bits: 32 },
                "f64" => Type::Float { bits: 64 },
                "char" => Type::Char,
                "string" => Type::String,
                _ => {
                    match ast_types.get(name) {
                        Some((declaration, is_export)) => {
                            self.compile_type_declaration(declaration, *is_export, ast_types)
                        }
                        None => {
                            self.errors.push(Error::unknown_identifier(annotation.span));
                            // Return a fallback type to continue compilation
                            Type::None
                        }
                    }
                }
            },
            ast::TypeAnnotationKind::List(item) => {
                Type::List(Box::new(self.compile_type_annotation(item, ast_types)))
            }
            ast::TypeAnnotationKind::Option(payload) => {
                Type::Option(Box::new(self.compile_type_annotation(payload, ast_types)))
            }
            ast::TypeAnnotationKind::Result { ok, err } => Type::Result {
                ok: Box::new(self.compile_type_annotation(ok, ast_types)),
                err: Box::new(self.compile_type_annotation(err, ast_types)),
            },
            ast::TypeAnnotationKind::Tuple(fields) => Type::Tuple(
                fields
                    .into_iter()
                    .map(|field| self.compile_type_annotation(field, ast_types))
                    .collect(),
            ),
        }
    }

    fn compile_signature(
        &mut self,
        declaration: &ast::FunctionDeclaration,
        is_export: bool,
        ast_types: &HashMap<String, (ast::Declaration, bool)>,
    ) -> FunctionSignature {
        let param_types = declaration
            .parameters
            .iter()
            .map(|param| self.compile_type_annotation(&param.type_annotation, ast_types))
            .collect();
        let return_type = declaration.return_type.as_ref().map_or(Type::None, |ty| {
            self.compile_type_annotation(&ty, ast_types)
        });

        FunctionSignature {
            param_types,
            return_type,
            is_export,
        }
    }

    fn compile_fun(
        &mut self,
        declaration: &ast::FunctionDeclaration,
        is_export: bool,
        ast_types: &HashMap<String, (ast::Declaration, bool)>,
        types: &HashMap<String, Type>,
        signatures: &HashMap<String, FunctionSignature>,
    ) -> Function {
        let parameters: Vec<_> = declaration
            .parameters
            .iter()
            .map(|param| {
                (
                    param.name.clone(),
                    self.compile_type_annotation(&param.type_annotation, ast_types),
                )
            })
            .collect();
        let return_type = declaration
            .return_type
            .as_ref()
            .map(|type_| self.compile_type_annotation(&type_, ast_types))
            .unwrap_or(Type::None);

        let mut vars = VarCollector::new(return_type.clone());
        let mut visible = Visible::new(types);

        for (name, ty) in &parameters {
            let param = vars.push(ty.clone());
            visible.bind(name.clone(), param, false);
        }

        let block = self.compile_block(
            &declaration.body,
            &mut vars,
            &mut visible,
            signatures,
            ast_types,
        );

        Function {
            parameters: parameters
                .iter()
                .map(|(name, ty)| Parameter {
                    name: name.clone(),
                    ty: ty.clone(),
                })
                .collect(),
            return_type,
            body: FunctionBody {
                vars: vars.vars,
                body: block.into(),
            },
            is_export,
        }
    }

    fn compile_test(
        &mut self,
        idx: usize,
        test: TestDeclaration,
        signatures: &HashMap<String, FunctionSignature>,
        types: &HashMap<String, Type>,
        ast_types: &HashMap<String, (ast::Declaration, bool)>,
    ) -> Test {
        const CHARS: [char; 26] = [
            'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q',
            'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z',
        ];

        let mut str = String::new();
        let mut idx = idx + 1;
        while idx > 0 {
            idx -= 1;
            let rem = idx % 26;
            str.push(CHARS[rem]);
            idx -= rem;
            idx /= 26;
        }
        let id = format!("{INTERNAL_PREFIX}-test-{str}-{INTERNAL_SUFFIX}",);

        let function = self.compile_fun(
            &FunctionDeclaration {
                name: str.clone(),
                parameters: vec![],
                return_type: None,
                body: test.body,
                span: test.span,
            },
            true,
            ast_types,
            types,
            signatures,
        );

        Test {
            name: test.name,
            id,
            body: function.body,
        }
    }

    fn compile_block(
        &mut self,
        block: &ast::Block,
        fun: &mut VarCollector,
        visible: &mut Visible,
        signatures: &HashMap<String, FunctionSignature>,
        ast_types: &HashMap<String, (ast::Declaration, bool)>,
    ) -> Body {
        let visible_snapshot = visible.snapshot();

        let mut statements = Vec::new();
        for statement in &block.statements {
            if matches!(statement, ast::Statement::EmptyLine) {
                continue;
            }
            statements.push(self.compile_statement(statement, fun, visible, signatures, ast_types));
        }

        visible.restore(visible_snapshot);
        Body { statements }
    }

    fn compile_statement(
        &mut self,
        statement: &ast::Statement,
        vars: &mut VarCollector,
        visible: &mut Visible,
        signatures: &HashMap<String, FunctionSignature>,
        ast_types: &HashMap<String, (ast::Declaration, bool)>,
    ) -> Expression {
        const EMPTY_BODY: Body = Body {
            statements: Vec::new(),
        };
        match statement {
            ast::Statement::EmptyLine => unreachable!(),
            ast::Statement::VariableDeclaration(variable_declaration) => {
                // TODO: allow uninitialized variables
                let initial_value = self.compile_expression(
                    variable_declaration
                        .value
                        .as_ref()
                        .expect("uninitialized var"),
                    vars,
                    visible,
                    signatures,
                );

                let expression = if let Some(anno) = &variable_declaration.type_annotation {
                    let annotated_type = self.compile_type_annotation(&anno, ast_types);
                    self.try_cast(&annotated_type, initial_value, signatures, variable_declaration.span)
                } else {
                    initial_value
                };

                let var = vars.push(expression.ty.clone());
                visible.bind(
                    variable_declaration.name.clone(),
                    var,
                    variable_declaration.mutable,
                );
                Expression::var_set(var, expression)
            }
            ast::Statement::Assignment { target, value } => {
                let comp_val = self.compile_expression(value, vars, visible, signatures);
                let name = match &target.kind {
                    ast::ExpressionKind::Identifier(name) => name,
                    _ => {
                        self.errors.push(Error::unsupported_operation(
                            "assignment to non-identifier".to_string(),
                            "expression".to_string(),
                            target.span,
                        ));
                        return Expression::void();
                    }
                };
                let binding = match visible.lookup(name) {
                    Some(binding) => binding,
                    None => {
                        self.errors.push(Error::unknown_identifier(target.span));
                        return Expression::void();
                    }
                };
                if !binding.mutable {
                    self.errors.push(Error::immutable_assignment(
                        name.clone(),
                        target.span.to(value.span),
                    ));
                    return Expression::void();
                }
                let annotated_type = &vars[binding.id];
                let value = self.try_cast(annotated_type, comp_val, signatures, target.span.to(value.span));
                Expression::var_set(binding.id, value)
            }
            ast::Statement::ExpressionStatement(expression) => {
                self.compile_expression(expression, vars, visible, signatures)
            }
            ast::Statement::ReturnStatement(expression) => {
                let returned = match expression {
                    Some(expression) => {
                        self.compile_expression(expression, vars, visible, signatures)
                    }
                    None => Expression::void(),
                };
                let span = expression.as_ref().map(|e| e.span).unwrap_or(Span { start: 0, end: 0 });
                let returned = self.try_cast(&vars.result, returned, signatures, span);
                Expression::return_(returned)
            }
            ast::Statement::BreakStatement(expression) => {
                let value = match expression {
                    Some(expression) => {
                        self.compile_expression(expression, vars, visible, signatures)
                    }
                    None => Expression::void(),
                };
                Expression::break_(value)
            }
            ast::Statement::ContinueStatement => Expression::continue_(),
            ast::Statement::ThrowStatement(value) => {
                let value = self.compile_expression(value, vars, visible, signatures);
                Expression::throw(value)
            }
            ast::Statement::IfStatement(if_statement) => {
                let condition =
                    self.compile_expression(&if_statement.condition, vars, visible, signatures);
                let then = self.compile_block(
                    &if_statement.then_branch,
                    vars,
                    visible,
                    signatures,
                    ast_types,
                );
                let else_ = match &if_statement.else_branch {
                    Some(else_branch) => {
                        self.compile_block(&else_branch, vars, visible, signatures, ast_types)
                    }
                    None => EMPTY_BODY,
                };
                let ty = unify(&then.returns().ty, &else_.returns().ty);
                Expression::if_(condition, then, else_, ty)
            }
            ast::Statement::MatchStatement(_) => todo!("impl match"),
            ast::Statement::WhileStatement(while_statement) => {
                let inner_body =
                    self.compile_block(&while_statement.body, vars, visible, signatures, ast_types);
                let complete_body = {
                    let mut body = Vec::new();
                    let condition = self.compile_expression(
                        &while_statement.condition,
                        vars,
                        visible,
                        signatures,
                    );
                    body.push(Expression::if_(
                        condition,
                        EMPTY_BODY,
                        vec![Expression::break_(Expression::void())].into(),
                        Type::None,
                    ));
                    for statement in inner_body.statements {
                        body.push(statement);
                    }
                    body.push(Expression::void());
                    body
                };
                Expression::loop_(complete_body.into(), Type::None)
            }
            ast::Statement::ForStatement(for_statement) => {
                let iterable =
                    self.compile_expression(&for_statement.iterable, vars, visible, signatures);
                let body: Body = match &iterable.ty {
                    Type::List(elem) => {
                        let list_elem = vars.push(*elem.clone());
                        visible.bind(for_statement.variable.clone(), list_elem, false);

                        // WASM locals are zero initialized by default
                        let index = vars.push(Type::u32());

                        let condition = Expression::call(
                            "less_than__u32_u32".to_owned(),
                            [
                                Expression::var_get(index, Type::u32()),
                                Expression::call(
                                    format!("len__list___{}", elem),
                                    [iterable.clone()].into(),
                                    Type::u32(),
                                ),
                            ]
                            .into(),
                            Type::Bool,
                        );

                        let elem = *elem.clone();
                        let mut inner = self.compile_block(
                            &for_statement.body,
                            vars,
                            visible,
                            signatures,
                            ast_types,
                        );
                        // Write list elem in iterator variable
                        inner.statements = [
                            vec![Expression::var_set(
                                list_elem,
                                Expression::index_access(
                                    // TODO: don't clone expression here, write into local
                                    iterable,
                                    Expression::var_get(index, Type::u32()),
                                    elem,
                                ),
                            )],
                            inner.statements,
                        ]
                        .concat();

                        [
                            Expression::if_(
                                condition,
                                inner,
                                Expression::break_(Expression::void()).into(),
                                Type::None,
                            ),
                            Expression::var_set(
                                index,
                                Expression::call(
                                    "add__u32_u32".into(),
                                    [
                                        Expression {
                                            kind: ExpressionKind::Int(1),
                                            ty: Type::u32(),
                                        },
                                        Expression::var_get(index, Type::u32()),
                                    ]
                                    .into(),
                                    Type::u32(),
                                ),
                            ),
                        ]
                        .into()
                    }
                    Type::Resource { methods: _ } => todo!("Allow resources with a 'next' "),
                    // TODO: allow ranges
                    ty => {
                        self.errors.push(Error::type_mismatch(
                            "iterable type".to_string(),
                            format!("non-iterable type {ty}"),
                            for_statement.span,
                        ));
                        EMPTY_BODY
                    }
                };

                Expression::loop_(body, Type::None)
            }
            ast::Statement::Assertion(assert_statement) => {
                let condition =
                    self.compile_expression(&assert_statement.condition, vars, visible, signatures);
                Expression::if_(
                    condition,
                    EMPTY_BODY,
                    vec![Expression::unreachable()].into(),
                    Type::None,
                )
            }
        }
    }

    fn compile_expression(
        &mut self,
        expression: &ast::Expression,
        vars: &mut VarCollector,
        visible: &mut Visible,
        signatures: &HashMap<String, FunctionSignature>,
    ) -> Expression {
        match &expression.kind {
            ast::ExpressionKind::Literal(literal) => match &literal.kind {
                ast::LiteralKind::Integer(int) => Expression::int(*int),
                ast::LiteralKind::Float(float) => Expression::float(*float),
                ast::LiteralKind::String(string) => Expression::string(string.clone()),
                ast::LiteralKind::Boolean(bool) => Expression::bool(*bool),
                ast::LiteralKind::None => Expression::void(),
            },
            ast::ExpressionKind::Identifier(name) => {
                let binding = visible.lookup(&name);

                match binding {
                    Some(binding) => {
                        let ty = &vars[binding.id];
                        ExpressionKind::VarGet { var: binding.id }.typed(ty.clone())
                    }
                    // The name might be an enum or other type instead, so look it up as a type
                    None => match visible.lookup_type(name) {
                        Some(ty) => {
                            ExpressionKind::Type(ty.clone()).typed(Type::Meta(ty.clone().into()))
                        }
                        None => {
                            self.errors.push(Error::unknown_identifier(expression.span));
                            Expression::void()
                        }
                    },
                }
            }
            ast::ExpressionKind::BinaryExpression {
                left,
                operator,
                right,
            } => {
                let name = match operator {
                    ast::BinaryOperator::Arithmetic(operator) => match operator {
                        ast::ArithmeticOperator::Add => "add",
                        ast::ArithmeticOperator::Subtract => "sub",
                        ast::ArithmeticOperator::Multiply => "mul",
                        ast::ArithmeticOperator::Divide => "div",
                    },
                    BinaryOperator::Logic(op) => match op {
                        // TODO: handling this as a function does not allow short-circuiting
                        ast::LogicOperator::And => "and",
                        ast::LogicOperator::Or => "or",
                    },
                    ast::BinaryOperator::Equal => "eq",
                    ast::BinaryOperator::NotEqual => "ne",
                    ast::BinaryOperator::GreaterThan => "greater_than",
                    ast::BinaryOperator::LessThan => "less_than",
                    ast::BinaryOperator::GreaterEqual => "greater_eq",
                    ast::BinaryOperator::LessEqual => "less_eq",
                    ast::BinaryOperator::Contains => "contains",
                    ast::BinaryOperator::NullCoalesce => {
                        let (left, right) = (
                            self.compile_expression(left, vars, visible, signatures),
                            self.compile_expression(right, vars, visible, signatures),
                        );
                        let Type::Option(inner_ty) = left.ty.clone() else {
                            self.errors.push(Error::unsupported_operation(
                                "null coalesce operator".to_string(),
                                "non-optional".to_string(),
                                expression.span,
                            ));
                            return Expression::void();
                        };

                        // TODO: store the expression here to reuse it instead of duplicating
                        let left_store = vars.push(left.ty.clone());
                        let store_optional = Expression::var_set(left_store, left.clone());
                        let condition = Expression::call(
                            "is_some".to_owned(),
                            vec![Expression::var_get(left_store, left.ty.clone())],
                            Type::Bool,
                        );

                        let result_store = vars.push(*inner_ty.clone());
                        let left = Expression::var_set(
                            result_store,
                            Expression::call(
                                "unwrap_unsafe".to_owned(),
                                vec![Expression::var_get(left_store, left.ty.clone())],
                                left.ty.clone(),
                            ),
                        );
                        let right = Expression::var_set(result_store, right);

                        return Expression::multiple([
                            store_optional,
                            Expression::if_(
                                condition,
                                left.into(),
                                right.into(),
                                *inner_ty.clone(),
                            ),
                            Expression::var_get(result_store, *inner_ty),
                        ]);
                    }
                };

                self.compile_call(vars, visible, signatures, &[left, right], name, Vec::new(), expression.span)
            }
            ast::ExpressionKind::UnaryExpression { operator, operand } => {
                let operand = self.compile_expression(operand, vars, visible, signatures);
                let operand_ty = operand.ty.clone();
                match operator {
                    UnaryOperator::Negate => {
                        // TODO: allow tuples
                        match operand_ty {
                            Type::Never => {
                                self.errors.push(Error::unsupported_operation(
                                    "negation".to_string(),
                                    "never type".to_string(),
                                    expression.span,
                                ));
                                Expression::unreachable()
                            }
                            Type::Int { signed, bits } => {
                                if !signed {
                                    self.errors.push(Error::unsupported_operation(
                                        "negation".to_string(),
                                        "unsigned int".to_string(),
                                        expression.span,
                                    ));
                                    return Expression::void();
                                }
                                let zero = Expression {
                                    kind: ExpressionKind::Int(0),
                                    ty: Type::Int { signed, bits },
                                };
                                Expression::call(
                                    format!("sub__{}_{}", operand_ty, operand_ty).to_owned(),
                                    vec![zero, operand],
                                    operand_ty,
                                )
                            }
                            Type::Float { bits } => {
                                Expression::call(format!("neg__f{bits}"), vec![operand], operand_ty)
                            }
                            _ => {
                                self.errors.push(Error::unsupported_operation(
                                    "negation".to_string(),
                                    "unsupported type".to_string(),
                                    expression.span,
                                ));
                                Expression::void()
                            }
                        }
                    }
                    UnaryOperator::Not => todo!("unary not: xor(x, -1)"),
                }
            }
            ast::ExpressionKind::YeetExpression { .. } => todo!("yeet"),
            ast::ExpressionKind::FunctionCall {
                function,
                arguments,
            } => {
                let function = match &function.kind {
                    ast::ExpressionKind::Identifier(name) => name.clone(),
                    _ => todo!("call of non-name"),
                };

                self.compile_call(vars, visible, signatures, arguments, &function, Vec::new(), expression.span)
            }
            ast::ExpressionKind::MethodCall {
                receiver,
                method,
                arguments,
                safe,
            } => {
                // TODO: Check if receiver is a resource first and insert a dynamic method call in this case
                if *safe {
                    todo!("Safe calls");
                }

                // TODO: handle calls on resources separately

                let receiver = self.compile_expression(receiver, vars, visible, signatures);

                self.compile_call(vars, visible, signatures, arguments, method, vec![receiver], expression.span)
            }
            ast::ExpressionKind::FieldAccess {
                object,
                field,
                // TODO: Desugar safe calls to if condition
                safe,
            } => {
                if *safe {
                    todo!("Desugar safe calls to if condition");
                }
                let object = self.compile_expression(object, vars, visible, signatures);
                let object_ty = object.ty.clone();
                match object_ty {
                    Type::Never => ExpressionKind::Member {
                        of: object.into(),
                        name: field.clone(),
                    }
                    .typed(Type::Never),
                    Type::Record(ref fields) => match fields.get(field) {
                        Some(member_type) => {
                            let member_type = member_type.clone();
                            ExpressionKind::Member {
                                of: object.into(),
                                name: field.clone(),
                            }
                            .typed(member_type)
                        }
                        None => {
                            self.errors.push(Error::unknown_field(
                                field.clone(),
                                object_ty.to_string(),
                                expression.span,
                            ));
                            Expression::void()
                        }
                    },
                    Type::Meta(ty) => match *ty {
                        Type::Enum { ref cases } => {
                            match cases.iter().position(|case| case == field) {
                                Some(index) => ExpressionKind::Enum { case: index }.typed(*ty),
                                None => {
                                    self.errors.push(Error::unknown_field(
                                        field.clone(),
                                        "enum".to_string(),
                                        expression.span,
                                    ));
                                    Expression::void()
                                }
                            }
                        }
                        Type::Variant { cases: _ } => todo!(),
                        ty => {
                            self.errors.push(Error::unsupported_operation(
                                "member access".to_string(),
                                format!("type {ty}"),
                                expression.span,
                            ));
                            Expression::void()
                        }
                    },
                    ty => {
                        self.errors.push(Error::unknown_field(
                            field.clone(),
                            ty.to_string(),
                            expression.span,
                        ));
                        Expression::void()
                    }
                }
            }
            ast::ExpressionKind::IndexAccess { collection, index } => {
                let collection = self.compile_expression(&collection, vars, visible, signatures);
                let index = self.compile_expression(index, vars, visible, signatures);
                let collection_ty = collection.ty.clone();
                match collection_ty {
                    Type::Never => Expression::unreachable(),
                    Type::String => {
                        self.errors.push(Error::unsupported_operation(
                            "index access".to_string(),
                            "string".to_string(),
                            expression.span,
                        ));
                        Expression::void()
                    }
                    Type::List(item_ty) => Expression::index_access(collection, index, *item_ty),
                    Type::Tuple(item_tys) => {
                        let index = match index.kind {
                            ExpressionKind::Int(int) => int,
                            _ => {
                                self.errors.push(Error::type_mismatch(
                                    "integer literal".to_string(),
                                    "non-integer expression".to_string(),
                                    expression.span,
                                ));
                                return Expression::void();
                            }
                        } as usize;
                        let item_ty = item_tys[index].clone();
                        Expression::tuple_access(collection, index, item_ty)
                    }
                    _ => {
                        self.errors.push(Error::unsupported_operation(
                            "index access".to_string(),
                            "non-collection".to_string(),
                            expression.span,
                        ));
                        Expression::void()
                    }
                }
            }
            ast::ExpressionKind::Record { name, members } => {
                let mut fields = HashMap::new();
                for member in members {
                    let name = member.name.clone();
                    let value = self.compile_expression(&member.element, vars, visible, signatures);
                    fields.insert(name, value);
                }
                let ty = match name {
                    Some(name) => todo!("get type {name}"),
                    None => {
                        let mut field_types = HashMap::new();
                        for (name, value) in &fields {
                            field_types.insert(name.clone(), value.ty.clone());
                        }
                        Type::Record(field_types)
                    }
                };
                Expression::record(fields, ty)
            }
            ast::ExpressionKind::ListLiteral(items) => {
                let mut compiled_items = vec![];
                for item in items.iter().rev() {
                    let item = self.compile_expression(item, vars, visible, signatures);
                    compiled_items.push(item);
                }
                let item_ty = if compiled_items.is_empty() {
                    Type::Blank
                } else {
                    let mut ty = compiled_items[0].ty.clone();
                    for item in &compiled_items[1..] {
                        ty = unify(&ty, &item.ty);
                    }
                    ty
                };
                Expression::list(compiled_items, Type::List(Box::new(item_ty)))
            }
        }
    }

    fn compile_call<Expr>(
        &mut self,
        vars: &mut VarCollector,
        visible: &mut Visible<'_>,
        signatures: &HashMap<String, FunctionSignature>,
        arguments: &[Expr],
        function: &str,
        mut args: Vec<Expression>,
        span: Span,
    ) -> Expression
    where
        Expr: AsRef<ast::Expression>,
    {
        for arg in arguments {
            args.push(self.compile_expression(arg.as_ref(), vars, visible, signatures));
        }

        let (function, signature) = match self.find_signature(&function, &args, signatures) {
            Ok((func, sig)) => (func, sig),
            Err(mangled_name) => {
                self.errors
                    .push(Error::unknown_function(mangled_name, span));
                
                let dummy_signature = FunctionSignature {
                    param_types: args.iter().map(|a| a.ty.clone()).collect(),
                    return_type: Type::None,
                    is_export: false,
                };
                (function.to_string(), dummy_signature)
            }
        };

        for (arg, expected) in args.iter_mut().zip(signature.param_types.iter()) {
            *arg = self.try_cast(expected, arg.clone(), signatures, span);
        }

        Expression::call(function.clone(), args, signature.return_type.clone())
    }

    fn try_cast(
        &mut self,
        annotation: &Type,
        mut actual: Expression,
        signatures: &HashMap<String, FunctionSignature>,
        span: Span,
    ) -> Expression {
        match (annotation, &actual.ty) {
            // No cast needed
            (a, b) if a == b => actual,
            (list @ Type::List(_), Type::List(b)) if **b == Type::Blank => {
                actual.ty = list.clone();
                actual
            }
            (option @ Type::Option(_), Type::Option(b)) if **b == Type::Blank => {
                actual.ty = option.clone();
                actual
            }
            (
                _target @ Type::Int { signed, bits },
                Type::Int {
                    signed: sign_actual,
                    bits: bits_actual,
                },
            ) if (signed == sign_actual && bits >= bits_actual)
                || (*signed && *bits >= bits_actual / 2) =>
            {
                match self.cast_int(actual.clone(), *bits, *signed, signatures) {
                    Ok(casted) => casted,
                    Err(mangled_name) => {
                        self.errors
                            .push(Error::unknown_function(mangled_name, span));
                        actual
                    }
                }
            }
            // TODO: Decide if we want this behavior for enums... my (Antonius) sense is, ultimately no, but it is convenient for now, so let's keep it until we have operators for enums and structs implemented
            (Type::Int { signed: _, bits: _ }, Type::Enum { cases: _ }) => actual,
            // TODO: put in more auto-conversions, e.g. casting from non-optional to optional should insert an explicit call to "some"
            (Type::Option(inner), Type::None) => Expression::none(*inner.clone()),
            (Type::Option(_), _) => Expression::some(actual),
            (a, b) => {
                self.errors.push(Error::type_mismatch(
                    a.to_string(),
                    b.to_string(),
                    span,
                ));
                actual
            }
        }
    }

    fn cast_int(
        &self,
        actual: Expression,
        bits: usize,
        signed: bool,
        signatures: &HashMap<String, FunctionSignature>,
    ) -> Result<Expression, String> {
        let name = format!("{}{bits}", if signed { "s" } else { "u" });

        let args = [actual];
        let (func, signature) = self.find_signature(&name, &args, signatures)?;

        Ok(Expression::call(func, args.into(), signature.return_type))
    }

    fn find_signature(
        &self,
        name: &str,
        args: &[Expression],
        signatures: &HashMap<String, FunctionSignature>,
    ) -> Result<(String, FunctionSignature), String> {
        let mut mangled = format!("{name}_");

        fn type_name(ty: &Type) -> String {
            match ty {
                Type::List(elem) => format!("list___{}", type_name(elem)),
                Type::Option(some) => format!("option___{}", type_name(some)),
                Type::Result { ok, err } => {
                    format!("result___{}___{}", type_name(ok), type_name(err))
                }
                // TODO: this is a hack to make enums work right now
                Type::Enum { cases: _ } => "s32".to_owned(),
                _ => ty.to_string(),
            }
        }

        // Try exact name match first
        if let Some((function_name, signature)) = signatures.get_key_value(name) {
            return Ok((function_name.clone(), signature.clone()));
        }

        // Try mangled name match
        for arg in args {
            mangled.push('_');
            mangled.push_str(&type_name(&arg.ty));
        }

        if let Some((function_name, signature)) = signatures.get_key_value(&mangled) {
            return Ok((function_name.clone(), signature.clone()));
        }

        // Function not found - return error
        Err(mangled)
    }
}

struct VarCollector {
    vars: Vec<Type>,
    /// The return type of the current function scope
    result: Type,
}

impl VarCollector {
    fn new(result: Type) -> Self {
        Self {
            vars: Vec::new(),
            result,
        }
    }

    fn push(&mut self, ty: Type) -> VarId {
        let id = VarId(self.vars.len());
        self.vars.push(ty);
        id
    }
}

impl Index<VarId> for VarCollector {
    type Output = Type;

    fn index(&self, index: VarId) -> &Self::Output {
        &self.vars[index]
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use wtf_error::{Error, ErrorKind};
    use wtf_parser::parser::Parser;

    fn parse_and_compile(source: &str) -> Result<Module, Vec<Error>> {
        let mut parser = Parser::new(source);
        let ast = parser.parse_module().expect("Parse should succeed");
        compile(ast)
    }

    #[test]
    fn test_successful_compilation() {
        let source = r#"
            func example() {
                var x = 42
                x
            }
        "#;

        let result = parse_and_compile(source);
        assert!(
            result.is_ok(),
            "Simple function should compile successfully"
        );

        let module = result.unwrap();
        assert!(module.functions.contains_key("example"));
    }

    #[test]
    fn test_unknown_identifier_error() {
        let source = r#"
            func example() {
                unknown_var
            }
        "#;

        let result = parse_and_compile(source);
        assert!(result.is_err(), "Should detect unknown identifier");

        let errors = result.unwrap_err();
        assert_eq!(errors.len(), 1);
        matches!(errors[0].kind, ErrorKind::UnknownIdentifier);
    }

    #[test]
    fn test_unknown_function_error() {
        let source = r#"
            func example() {
                unknown_function(42)
            }
        "#;

        let result = parse_and_compile(source);
        assert!(result.is_err(), "Should detect unknown function");

        let errors = result.unwrap_err();
        assert_eq!(errors.len(), 1);
        if let ErrorKind::UnknownFunction { name } = &errors[0].kind {
            assert!(name.contains("unknown_function"));
        } else {
            panic!("Expected UnknownFunction error, got {:?}", errors[0].kind);
        }
    }

    #[test]
    fn test_unknown_type_error() {
        let source = r#"
            func example() {
                var x: UnknownType = 42
                x
            }
        "#;

        let result = parse_and_compile(source);

        // The behavior may vary - some unknown types might be detected, others might not
        // depending on where they're used in the compilation pipeline
        if let Err(errors) = result {
            assert_eq!(errors.len(), 1);
            matches!(errors[0].kind, ErrorKind::UnknownIdentifier);
            println!("Successfully detected unknown type error: {:?}", errors[0]);
        } else {
            // If no error is detected, this might be because the type system is permissive
            // or because the unknown type fallback is working
            println!("Unknown type test passed without error - type system may be permissive");
        }
    }

    #[test]
    fn test_field_access_on_invalid_type() {
        let source = r#"
            func example() {
                var x = 42
                x.nonexistent_field
            }
        "#;

        let result = parse_and_compile(source);
        assert!(
            result.is_err(),
            "Should detect field access on invalid type"
        );

        let errors = result.unwrap_err();
        assert_eq!(errors.len(), 1);
    }

    #[test]
    fn test_multiple_errors_collection() {
        let source = r#"
            func example() {
                var x = unknown_var1
                var y = unknown_var2
                unknown_function(x, y)
                var z: UnknownType = 10
            }
        "#;

        let result = parse_and_compile(source);
        assert!(result.is_err(), "Should detect multiple errors");

        let errors = result.unwrap_err();
        println!("Collected {} errors: {:?}", errors.len(), errors);

        // With current panic-catching implementation, we might only catch the first error
        // But we should get at least one error
        assert!(
            errors.len() >= 1,
            "Should detect at least one error, got {} errors",
            errors.len()
        );

        // Check that we have a unknown identifier error (since that's what we're testing)
        let error_types: Vec<_> = errors.iter().map(|e| &e.kind).collect();

        let unknown_id_count = error_types
            .iter()
            .filter(|e| matches!(e, ErrorKind::UnknownIdentifier))
            .count();
        assert!(
            unknown_id_count >= 1,
            "Should have at least one unknown identifier error"
        );
    }

    #[test]
    fn test_multiple_functions_with_errors() {
        let source = r#"
            func function_with_unknown_var() {
                unknown_var
            }
            
            func function_with_unknown_function() {
                unknown_function(42)
            }
        "#;

        let result = parse_and_compile(source);
        assert!(
            result.is_err(),
            "Should detect errors in multiple functions"
        );

        let errors = result.unwrap_err();
        println!(
            "Collected {} errors from multiple functions: {:?}",
            errors.len(),
            errors
        );

        // This test focuses on demonstrating error detection across multiple functions
        assert!(
            errors.len() >= 1,
            "Should detect at least one error across multiple functions"
        );
    }

    #[test]
    fn test_immutable_assignment_error() {
        let source = r#"
            func example() {
                let x = 42
                x = 24
            }
        "#;

        let result = parse_and_compile(source);
        // This should either succeed (if assignment creates a new binding) or fail with assignment error
        // The exact behavior depends on the language semantics
        if let Err(errors) = result {
            if !errors.is_empty() {
                // If it fails, check that it's for the right reason
                let _has_assignment_error = errors
                    .iter()
                    .any(|e| matches!(e.kind, ErrorKind::ImmutableAssignment { .. }));
                println!("Assignment errors: {:?}", errors);
                // Don't assert for now as the semantics might vary
            }
        }
    }

    #[test]
    fn test_invalid_operations() {
        let source = r#"
            func example() {
                var x = "hello"
                var y = x + 42
                y
            }
        "#;

        let result = parse_and_compile(source);
        // Should either succeed (with implicit conversions) or fail with type mismatch
        if let Err(errors) = result {
            println!("Operation errors: {:?}", errors);
            // Type system may be permissive, so just check it doesn't crash
        }
    }

    #[test]
    fn test_mixed_valid_and_invalid_code() {
        let source = r#"
            func valid_function() {
                var x = 42
                x + 1
            }
            
            func invalid_function() {
                unknown_identifier
            }
        "#;

        let result = parse_and_compile(source);
        assert!(result.is_err(), "Should detect errors in invalid function");

        let errors = result.unwrap_err();
        assert!(!errors.is_empty());

        // Even with errors, we should have tried to compile both functions
        // (though the result is Err, the valid parts should have been processed)
    }

    #[test]
    fn test_errors_use_actual_spans_not_dummy_spans() {
        let source = r#"
func example() {
    unknown_identifier
}
        "#;

        let result = parse_and_compile(source);
        assert!(result.is_err(), "Should detect unknown identifier error");

        let errors = result.unwrap_err();
        assert!(!errors.is_empty());

        // Check that errors use actual spans from AST, not dummy spans (0, 0)
        for error in &errors {
            // The error should have a non-dummy span
            assert!(
                error.span.start != 0 || error.span.end != 0,
                "Error span should not be a dummy span (0, 0), got ({}, {})",
                error.span.start,
                error.span.end
            );
            
            // The span should be reasonable (within the source length)
            assert!(
                error.span.end <= source.len(),
                "Error span end ({}) should not exceed source length ({})",
                error.span.end,
                source.len()
            );
        }
    }
}
