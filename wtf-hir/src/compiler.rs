use std::collections::HashMap;

use wtf_ast::{self as ast, BinaryOperator, TypeAnnotation};

use crate::{
    get, visible::Visible, Expression, ExpressionKind, Function, FunctionSignature, Id, Module,
    PrimitiveType, ResourceType, Type,
};

pub fn compile(ast: ast::Module) -> Module {
    let mut ast_types = HashMap::new();
    let mut ast_funs = vec![];
    for declaration in ast.declarations {
        match declaration {
            ast::Declaration::Function(fun) => {
                ast_funs.push(fun);
            }
            ast::Declaration::Record(rec) => {
                ast_types.insert(rec.name.to_string(), ast::Declaration::Record(rec));
            }
            ast::Declaration::Resource(res) => {
                ast_types.insert(res.name.to_string(), ast::Declaration::Resource(res));
            }
            ast::Declaration::Enum(en) => {
                ast_types.insert(en.name.to_string(), ast::Declaration::Enum(en));
            }
            ast::Declaration::Variant(var) => {
                ast_types.insert(var.name.to_string(), ast::Declaration::Variant(var));
            }
            ast::Declaration::Export(_) => todo!(),
        }
    }

    let mut functions = HashMap::new();
    for fun in &ast_funs {
        functions.insert(fun.name.to_string(), compile_fun(fun, &ast_types));
    }
    Module {
        types: HashMap::new(),
        functions,
    }
}
fn compile_type_declaration(
    declaration: &ast::Declaration,
    ast_types: &HashMap<String, ast::Declaration>,
) -> Type {
    let type_ = match declaration {
        ast::Declaration::Record(record) => {
            let mut fields = HashMap::new();
            for field in &record.fields {
                fields.insert(
                    field.name.to_string(),
                    compile_type_annotation(&field.type_annotation, ast_types),
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
                        .unwrap_or_else(|| TypeAnnotation::Simple("Nothing".to_string()));
                    compile_type_annotation(&annotation, ast_types)
                };
                methods.insert(
                    method.name.clone(),
                    FunctionSignature {
                        param_types: method
                            .parameters
                            .iter()
                            .map(|param| compile_type_annotation(&param.type_annotation, ast_types))
                            .collect(),
                        return_type,
                    },
                );
            }
            Type::Resource(ResourceType { methods })
        }
        ast::Declaration::Enum(enum_) => {
            Type::Enum(enum_.cases.iter().map(|case| case.clone()).collect())
        }
        ast::Declaration::Variant(variants) => {
            let mut result = HashMap::new();
            for variant in &variants.cases {
                let mut fields = HashMap::new();
                for field in &variant.associated_types {
                    fields.insert(
                        field.name.clone(),
                        compile_type_annotation(&field.type_annotation, ast_types),
                    );
                }
                result.insert(variant.name.clone(), fields);
            }
            Type::Variant(result)
        }
        _ => unreachable!(),
    };
    type_
}
fn compile_type_annotation(
    annotation: &ast::TypeAnnotation,
    ast_types: &HashMap<String, ast::Declaration>,
) -> Type {
    match annotation {
        ast::TypeAnnotation::Simple(name) => Type::Builtin(match name.as_str() {
            "bool" => PrimitiveType::Bool,
            "s8" => PrimitiveType::S8,
            "s16" => PrimitiveType::S16,
            "s32" => PrimitiveType::S32,
            "s64" => PrimitiveType::S64,
            "u8" => PrimitiveType::U8,
            "u16" => PrimitiveType::U16,
            "u32" => PrimitiveType::U32,
            "u64" => PrimitiveType::U64,
            "f32" => PrimitiveType::F32,
            "f64" => PrimitiveType::F64,
            "Char" => PrimitiveType::Char,
            "String" => PrimitiveType::String,
            _ => {
                let declaration = ast_types
                    .get(name)
                    .unwrap_or_else(|| panic!("unknown type {name}"));
                return compile_type_declaration(declaration, ast_types);
            }
        }),
        ast::TypeAnnotation::List(item) => {
            Type::List(Box::new(compile_type_annotation(item, ast_types)))
        }
        ast::TypeAnnotation::Option(payload) => {
            Type::Option(Box::new(compile_type_annotation(payload, ast_types)))
        }
        ast::TypeAnnotation::Result { ok, err } => Type::Result {
            ok: Box::new(compile_type_annotation(ok, ast_types)),
            err: Box::new(compile_type_annotation(err, ast_types)),
        },
        ast::TypeAnnotation::Tuple(fields) => Type::Tuple(
            fields
                .into_iter()
                .map(|field| compile_type_annotation(field, ast_types))
                .collect(),
        ),
    }
}

fn compile_fun(
    declaration: &ast::FunctionDeclaration,
    ast_types: &HashMap<String, ast::Declaration>,
) -> Function {
    let parameters = declaration
        .parameters
        .iter()
        .map(|param| {
            (
                param.name.clone(),
                compile_type_annotation(&param.type_annotation, ast_types),
            )
        })
        .collect();
    let return_type = declaration
        .return_type
        .as_ref()
        .map(|type_| compile_type_annotation(&type_, ast_types))
        .unwrap_or(Type::None);
    let (expressions, body) = {
        let mut visible = Visible::new();
        let mut expressions = vec![];
        let mut body = vec![];

        for param in &declaration.parameters {
            let type_ = compile_type_annotation(&param.type_annotation, ast_types);
            visible.bind(
                param.name.to_string(),
                register(&mut expressions, &mut body, Expression::param(type_)),
            );
        }

        let inner_body = compile_block(&declaration.body, &mut visible, &mut expressions);
        let expr = get(&expressions, inner_body);
        let ExpressionKind::Block { children, result } = expr.kind else {
            unreachable!()
        };
        body.extend(children);
        let body = register(
            &mut expressions,
            &mut vec![],
            Expression::block(body, result, expr.type_),
        );

        (expressions, body)
    };

    Function {
        parameters,
        return_type,
        expressions,
        body,
    }
}

fn register(expressions: &mut Vec<Expression>, body: &mut Vec<Id>, expr: Expression) -> Id {
    let id = Id(expressions.len());
    expressions.push(expr);
    body.push(id);
    id
}

fn compile_block(block: &ast::Block, visible: &mut Visible, exprs: &mut Vec<Expression>) -> Id {
    let mut body = vec![];
    for statement in &block.statements {
        compile_statement(statement, visible, exprs, &mut body);
    }
    if body.is_empty() {
        register(exprs, &mut body, Expression::none());
    }
    let return_expr = *body.last().unwrap();
    let return_type = get(exprs, return_expr).type_;
    register(
        exprs,
        &mut vec![],
        Expression::block(body, return_expr, return_type),
    )
}
fn compile_statement(
    statement: &ast::Statement,
    visible: &mut Visible,
    exprs: &mut Vec<Expression>,
    body: &mut Vec<Id>,
) -> Id {
    match statement {
        ast::Statement::VariableDeclaration(variable_declaration) => {
            // TODO: allow uninitialized variables
            let value = compile_expression(
                variable_declaration.value.as_ref().unwrap(),
                visible,
                exprs,
                body,
            );
            visible.bind(variable_declaration.name.clone(), value);
            register(exprs, body, Expression::none())
        }
        ast::Statement::Assignment { .. } => {
            todo!()
        }
        ast::Statement::ExpressionStatement(expression) => {
            compile_expression(expression, visible, exprs, body)
        }
        ast::Statement::ReturnStatement(expression) => {
            let id = match expression {
                Some(expression) => compile_expression(expression, visible, exprs, body),
                None => register(exprs, body, Expression::none()),
            };
            register(exprs, body, Expression::return_(id))
        }
        ast::Statement::BreakStatement(expression) => {
            let id = match expression {
                Some(expression) => compile_expression(expression, visible, exprs, body),
                None => register(exprs, body, Expression::none()),
            };
            register(exprs, body, Expression::break_(id))
        }
        ast::Statement::ContinueStatement => register(exprs, body, Expression::continue_()),
        ast::Statement::ThrowStatement(_) => todo!(),
        ast::Statement::IfStatement(if_statement) => {
            let condition = compile_expression(&if_statement.condition, visible, exprs, body);
            let then = compile_block(&if_statement.then_branch, visible, exprs);
            let else_ = match &if_statement.else_branch {
                Some(expression) => compile_block(&expression, visible, exprs),
                None => register(exprs, body, Expression::none()),
            };
            // TODO: check that the types of the branches match
            register(exprs, body, Expression::if_(condition, then, else_))
        }
        ast::Statement::MatchStatement(_) => todo!(),
        ast::Statement::WhileStatement(while_statement) => {
            let mut inner_body = vec![];
            let condition =
                compile_expression(&while_statement.condition, visible, exprs, &mut inner_body);
            let none = register(exprs, &mut inner_body, Expression::none());
            let then_body = compile_block(&while_statement.body, visible, exprs);
            let else_body = none;
            let if_ = register(
                exprs,
                &mut inner_body,
                Expression::if_(condition, then_body, else_body),
            );
            register(exprs, body, Expression::loop_(if_))
        }
        ast::Statement::ForStatement(_) => todo!(),
    }
}
fn compile_expression(
    expression: &ast::Expression,
    visible: &mut Visible,
    exprs: &mut Vec<Expression>,
    body: &mut Vec<Id>,
) -> Id {
    match expression {
        ast::Expression::Literal(literal) => match literal {
            ast::Literal::Integer(int) => register(exprs, body, Expression::int(*int)),
            ast::Literal::Float(float) => register(exprs, body, Expression::float(*float)),
            ast::Literal::String(string) => {
                register(exprs, body, Expression::string(string.clone()))
            }
            ast::Literal::Boolean(bool) => {
                let none = register(exprs, body, Expression::none());
                register(exprs, body, Expression::bool(*bool, none))
            }
            ast::Literal::None => register(exprs, body, Expression::none()),
        },
        ast::Expression::Identifier(name) => visible
            .lookup(&name)
            .expect(&format!("Variable {} is not defined.", name)),
        ast::Expression::BinaryExpression {
            left,
            operator,
            right,
        } => {
            let left = compile_expression(left, visible, exprs, body);
            let right = compile_expression(right, visible, exprs, body);
            compile_operation(left, right, *operator, body, exprs)
        }
        ast::Expression::UnaryExpression { .. } => todo!(),
        ast::Expression::YeetExpression { .. } => todo!(),
        ast::Expression::FunctionCall {
            function,
            arguments,
        } => {
            let function = match function.as_ref() {
                ast::Expression::Identifier(name) => name.to_string(),
                _ => panic!("You can only call names."),
            };
            let mut args = vec![];
            for arg in arguments {
                args.push(compile_expression(arg, visible, exprs, body));
            }
            register(exprs, body, Expression::call(function, args))
        }
        ast::Expression::MethodCall { .. } => todo!(),
        ast::Expression::FieldAccess { .. } => todo!(),
        ast::Expression::IndexAccess { .. } => todo!(),
        ast::Expression::Record { .. } => todo!(),
        ast::Expression::ListLiteral(_) => todo!(),
    }
}

fn compile_operation(
    left: Id,
    right: Id,
    operator: BinaryOperator,
    body: &mut Vec<Id>,
    exprs: &mut Vec<Expression>,
) -> Id {
    match operator {
        ast::BinaryOperator::Arithmetic(op) => match op {
            ast::ArithmeticOperator::Add => register(
                exprs,
                body,
                Expression::call("add".to_string(), vec![left, right]),
            ),
            ast::ArithmeticOperator::Subtract => register(
                exprs,
                body,
                Expression::call("sub".to_string(), vec![left, right]),
            ),
            ast::ArithmeticOperator::Multiply => register(
                exprs,
                body,
                Expression::call("mul".to_string(), vec![left, right]),
            ),
            ast::ArithmeticOperator::Divide => register(
                exprs,
                body,
                Expression::call("div".to_string(), vec![left, right]),
            ),
        },
        ast::BinaryOperator::Equal => register(
            exprs,
            body,
            Expression::call("equal".to_string(), vec![left, right]),
        ),
        ast::BinaryOperator::NotEqual => register(
            exprs,
            body,
            Expression::call("not_equal".to_string(), vec![left, right]),
        ),
        ast::BinaryOperator::GreaterThan => register(
            exprs,
            body,
            Expression::call("greater_than".to_string(), vec![left, right]),
        ),
        ast::BinaryOperator::LessThan => register(
            exprs,
            body,
            Expression::call("less_than".to_string(), vec![left, right]),
        ),
        ast::BinaryOperator::GreaterEqual => register(
            exprs,
            body,
            Expression::call("greater_equal".to_string(), vec![left, right]),
        ),
        ast::BinaryOperator::LessEqual => register(
            exprs,
            body,
            Expression::call("less_equal".to_string(), vec![left, right]),
        ),
        ast::BinaryOperator::Contains => register(
            exprs,
            body,
            Expression::call("contains".to_string(), vec![left, right]),
        ),
        ast::BinaryOperator::NullCoalesce => todo!(),
    }
}
