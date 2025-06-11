use std::collections::HashMap;
use std::ops::{Deref, Index};

use wtf_ast::{
    self as ast, BinaryOperator, FunctionDeclaration, TestDeclaration, TypeAnnotation,
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

pub fn compile(ast: ast::Module) -> Result<Module, Vec<Error>> {
    let mut errors = Vec::new();
    let module = compile_internal(ast, &mut errors);
    
    if errors.is_empty() {
        Ok(module)
    } else {
        Err(errors)
    }
}

fn compile_internal(ast: ast::Module, errors: &mut Vec<Error>) -> Module {
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
            ast::Declaration::Overload(overload) => {
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
                let dummy_span = Span { start: 0, end: 0 };
                errors.push(Error::type_mismatch(
                    "single export declaration".to_string(),
                    "double export declaration".to_string(),
                    dummy_span,
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
            compile_type_declaration(decl, *is_export, &ast_types),
        );
    }

    let mut signatures = HashMap::with_builtins();
    // let mut keys = signatures.keys().collect::<Vec<_>>();
    // keys.sort();
    // println!("{:#?}", keys);
    for (fun, is_export) in ast_funs.values() {
        signatures.insert(
            fun.name.to_string(),
            compile_signature(fun, *is_export, &ast_types),
        );
    }

    // for (name, signature) in &signatures {
    //     println!("{name}");
    //     println!("{:#?}", signature);
    // }

    let mut functions = HashMap::new();
    for (fun, is_export) in ast_funs.values() {
        functions.insert(
            fun.name.to_string(),
            compile_fun(fun, *is_export, &ast_types, &types, &signatures),
        );
    }

    let mut tests = Vec::new();
    for (idx, test) in ast_tests.into_iter().enumerate() {
        tests.push(compile_test(idx, test, &signatures, &types, &ast_types));
    }

    Module {
        types,
        functions,
        tests,
    }
}

fn compile_type_declaration(
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
                        .unwrap_or_else(|| TypeAnnotation::Simple("none".to_string()));
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
                        compile_type_annotation(&field.type_annotation, ast_types),
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
    annotation: &ast::TypeAnnotation,
    ast_types: &HashMap<String, (ast::Declaration, bool)>,
) -> Type {
    match annotation {
        ast::TypeAnnotation::Simple(name) => match name.as_str() {
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
                let (declaration, is_export) = ast_types
                    .get(name)
                    .unwrap_or_else(|| panic!("unknown type {name}"));
                compile_type_declaration(declaration, *is_export, ast_types)
            }
        },
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
                compile_type_annotation(&param.type_annotation, ast_types),
            )
        })
        .collect();
    let return_type = declaration
        .return_type
        .as_ref()
        .map(|type_| compile_type_annotation(&type_, ast_types))
        .unwrap_or(Type::None);

    let mut vars = VarCollector::new(return_type.clone());
    let mut visible = Visible::new(types);

    for (name, ty) in &parameters {
        let param = vars.push(ty.clone());
        visible.bind(name.clone(), param, false);
    }

    let block = compile_block(
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

fn compile_signature(
    declaration: &ast::FunctionDeclaration,
    is_export: bool,
    ast_types: &HashMap<String, (ast::Declaration, bool)>,
) -> FunctionSignature {
    let param_types = declaration
        .parameters
        .iter()
        .map(|param| compile_type_annotation(&param.type_annotation, ast_types))
        .collect();
    let return_type = declaration
        .return_type
        .as_ref()
        .map_or(Type::None, |ty| compile_type_annotation(&ty, ast_types));

    FunctionSignature {
        param_types,
        return_type,
        is_export,
    }
}

fn compile_test(
    idx: usize,
    test: TestDeclaration,
    signatures: &HashMap<String, FunctionSignature>,
    types: &HashMap<String, Type>,
    ast_types: &HashMap<String, (ast::Declaration, bool)>,
) -> Test {
    const CHARS: [char; 26] = [
        'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r',
        's', 't', 'u', 'v', 'w', 'x', 'y', 'z',
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

    let function = compile_fun(
        &FunctionDeclaration {
            name: str.clone(),
            parameters: vec![],
            return_type: None,
            body: test.body,
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

fn compile_block(
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
        statements.push(compile_statement(
            statement, fun, visible, signatures, ast_types,
        ));
    }

    visible.restore(visible_snapshot);
    Body { statements }
}

fn compile_statement(
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
            let initial_value = compile_expression(
                variable_declaration
                    .value
                    .as_ref()
                    .expect("uninitialized var"),
                vars,
                visible,
                signatures,
            );

            let expression = if let Some(anno) = &variable_declaration.type_annotation {
                let annotated_type = compile_type_annotation(&anno, ast_types);
                try_cast(&annotated_type, initial_value, signatures)
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
            let value = compile_expression(value, vars, visible, signatures);
            let name = match target {
                ast::Expression::Identifier(name) => name,
                _ => panic!("Can only assign to names (for now)"), // TODO
            };
            let binding = visible.lookup(name).expect("Name {name} not defined");
            if !binding.mutable {
                panic!("Tried assigning to a mutable variable.");
            }
            let annotated_type = &vars[binding.id];
            let value = try_cast(annotated_type, value, signatures);
            Expression::var_set(binding.id, value)
        }
        ast::Statement::ExpressionStatement(expression) => {
            compile_expression(expression, vars, visible, signatures)
        }
        ast::Statement::ReturnStatement(expression) => {
            let returned = match expression {
                Some(expression) => compile_expression(expression, vars, visible, signatures),
                None => Expression::void(),
            };
            let returned = try_cast(&vars.result, returned, signatures);
            Expression::return_(returned)
        }
        ast::Statement::BreakStatement(expression) => {
            let value = match expression {
                Some(expression) => compile_expression(expression, vars, visible, signatures),
                None => Expression::void(),
            };
            Expression::break_(value)
        }
        ast::Statement::ContinueStatement => Expression::continue_(),
        ast::Statement::ThrowStatement(value) => {
            let value = compile_expression(value, vars, visible, signatures);
            Expression::throw(value)
        }
        ast::Statement::IfStatement(if_statement) => {
            let condition = compile_expression(&if_statement.condition, vars, visible, signatures);
            let then = compile_block(
                &if_statement.then_branch,
                vars,
                visible,
                signatures,
                ast_types,
            );
            let else_ = match &if_statement.else_branch {
                Some(else_branch) => {
                    compile_block(&else_branch, vars, visible, signatures, ast_types)
                }
                None => EMPTY_BODY,
            };
            let ty = unify(&then.returns().ty, &else_.returns().ty);
            Expression::if_(condition, then, else_, ty)
        }
        ast::Statement::MatchStatement(_) => todo!("impl match"),
        ast::Statement::WhileStatement(while_statement) => {
            let inner_body =
                compile_block(&while_statement.body, vars, visible, signatures, ast_types);
            let complete_body = {
                let mut body = Vec::new();
                let condition =
                    compile_expression(&while_statement.condition, vars, visible, signatures);
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
            let iterable = compile_expression(&for_statement.iterable, vars, visible, signatures);
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
                    let mut inner =
                        compile_block(&for_statement.body, vars, visible, signatures, ast_types);
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
                ty => panic!("Non-iterable type {ty}"),
            };

            Expression::loop_(body, Type::None)
        }
        ast::Statement::Assertion(assert_statement) => {
            let condition =
                compile_expression(&assert_statement.condition, vars, visible, signatures);
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
    expression: &ast::Expression,
    vars: &mut VarCollector,
    visible: &mut Visible,
    signatures: &HashMap<String, FunctionSignature>,
) -> Expression {
    match expression {
        ast::Expression::Literal(literal) => match literal {
            ast::Literal::Integer(int) => Expression::int(*int),
            ast::Literal::Float(float) => Expression::float(*float),
            ast::Literal::String(string) => Expression::string(string.clone()),
            ast::Literal::Boolean(bool) => Expression::bool(*bool),
            ast::Literal::None => Expression::void(),
        },
        ast::Expression::Identifier(name) => {
            let binding = visible.lookup(&name);

            match binding {
                Some(binding) => {
                    let ty = &vars[binding.id];
                    ExpressionKind::VarGet { var: binding.id }.typed(ty.clone())
                }
                // The name might be an enum or other type instead, so look it up as a type
                None => {
                    let ty = visible
                        .lookup_type(name)
                        .expect(&format!("Variable {} is not defined.", name));

                    ExpressionKind::Type(ty.clone()).typed(Type::Meta(ty.clone().into()))
                }
            }
        }
        ast::Expression::BinaryExpression {
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
                        compile_expression(left, vars, visible, signatures),
                        compile_expression(right, vars, visible, signatures),
                    );
                    let Type::Option(inner_ty) = left.ty.clone() else {
                        panic!("Can only use '?' on optionals")
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
                        Expression::if_(condition, left.into(), right.into(), *inner_ty.clone()),
                        Expression::var_get(result_store, *inner_ty),
                    ]);
                }
            };

            compile_call(vars, visible, signatures, &[left, right], name, Vec::new())
        }
        ast::Expression::UnaryExpression { operator, operand } => {
            let operand = compile_expression(operand, vars, visible, signatures);
            let operand_ty = operand.ty.clone();
            match operator {
                UnaryOperator::Negate => {
                    // TODO: allow tuples
                    match operand_ty {
                        Type::Never => panic!("negated never"),
                        Type::Int { signed, bits } => {
                            if !signed {
                                panic!("negating unsigned int")
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
                        _ => panic!("negating unsupported type"),
                    }
                }
                UnaryOperator::Not => todo!("unary not: xor(x, -1)"),
            }
        }
        ast::Expression::YeetExpression { .. } => todo!("yeet"),
        ast::Expression::FunctionCall {
            function,
            arguments,
        } => {
            let function = match function.deref() {
                ast::Expression::Identifier(name) => name.clone(),
                _ => todo!("call of non-name"),
            };

            compile_call(vars, visible, signatures, arguments, &function, Vec::new())
        }
        ast::Expression::MethodCall {
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

            let receiver = compile_expression(receiver, vars, visible, signatures);

            compile_call(vars, visible, signatures, arguments, method, vec![receiver])
        }
        ast::Expression::FieldAccess {
            object,
            field,
            // TODO: Desugar safe calls to if condition
            safe,
        } => {
            if *safe {
                todo!("Desugar safe calls to if condition");
            }
            let object = compile_expression(object, vars, visible, signatures);
            let object_ty = object.ty.clone();
            match object_ty {
                Type::Never => ExpressionKind::Member {
                    of: object.into(),
                    name: field.clone(),
                }
                .typed(Type::Never),
                Type::Record(fields) => {
                    let member_type = fields
                        .get(field)
                        .expect("No field named {field} on {ty}.")
                        .clone();

                    ExpressionKind::Member {
                        of: object.into(),
                        name: field.clone(),
                    }
                    .typed(member_type)
                }
                Type::Meta(ty) => match *ty {
                    Type::Enum { ref cases } => {
                        let index = cases
                            .iter()
                            .position(|case| case == field)
                            .expect(format!("Enum has no case '{field}'").as_str());

                        ExpressionKind::Enum { case: index }.typed(*ty)
                    }
                    Type::Variant { cases: _ } => todo!(),
                    ty => panic!("Cannot use member access syntax on type {ty}"),
                },
                ty => panic!("field access on non-struct and non-type: {ty}"),
            }
        }
        ast::Expression::IndexAccess { collection, index } => {
            let collection = compile_expression(&collection, vars, visible, signatures);
            let index = compile_expression(index, vars, visible, signatures);
            let collection_ty = collection.ty.clone();
            match collection_ty {
                Type::Never => Expression::unreachable(),
                Type::String => panic!("index access on string"),
                Type::List(item_ty) => Expression::index_access(collection, index, *item_ty),
                Type::Tuple(item_tys) => {
                    let index = match index.kind {
                        ExpressionKind::Int(int) => int,
                        _ => panic!("index access on tuple has to be with an int literal"),
                    } as usize;
                    let item_ty = item_tys[index].clone();
                    Expression::tuple_access(collection, index, item_ty)
                }
                _ => panic!("index access on collection"),
            }
        }
        ast::Expression::Record { name, members } => {
            let mut fields = HashMap::new();
            for member in members {
                let name = member.name.clone();
                let value = compile_expression(&member.element, vars, visible, signatures);
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
        ast::Expression::ListLiteral(items) => {
            let mut compiled_items = vec![];
            for item in items.iter().rev() {
                let item = compile_expression(item, vars, visible, signatures);
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
    vars: &mut VarCollector,
    visible: &mut Visible<'_>,
    signatures: &HashMap<String, FunctionSignature>,
    arguments: &[Expr],
    function: &str,
    mut args: Vec<Expression>,
) -> Expression
where
    Expr: AsRef<ast::Expression>,
{
    for arg in arguments {
        args.push(compile_expression(arg.as_ref(), vars, visible, signatures));
    }

    let (function, signature) = find_signature(&function, &args, signatures);

    for (arg, expected) in args.iter_mut().zip(signature.param_types.iter()) {
        *arg = try_cast(expected, arg.clone(), signatures);
    }

    Expression::call(function.clone(), args, signature.return_type.clone())
}

/// Tries to add an implicit cast that converts the given expression to the annotated type
fn try_cast(
    annotation: &Type,
    mut actual: Expression,
    signatures: &HashMap<String, FunctionSignature>,
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
            target @ Type::Int { signed, bits },
            Type::Int {
                signed: sign_actual,
                bits: bits_actual,
            },
        ) if (signed == sign_actual && bits >= bits_actual)
            || (*signed && *bits >= bits_actual / 2) =>
        {
            cast_int(actual, *bits, *signed, signatures)
        }
        // TODO: Decide if we want this behavior for enums... my (Antonius) sense is, ultimately no, but it is convenient for now, so let's keep it until we have operators for enums and structs implemented
        (Type::Int { signed: _, bits: _ }, Type::Enum { cases: _ }) => actual,
        // TODO: put in more auto-conversions, e.g. casting from non-optional to optional should insert an explicit call to "some"
        (Type::Option(inner), Type::None) => Expression::none(*inner.clone()),
        (Type::Option(_), _) => Expression::some(actual),
        (a, b) => panic!("Cannot implicitly cast {b} into {a}"),
    }
}

fn cast_int(
    actual: Expression,
    bits: usize,
    signed: bool,
    signatures: &HashMap<String, FunctionSignature>,
) -> Expression {
    let name = format!("{}{bits}", if signed { "s" } else { "u" });

    let args = [actual];
    let (func, signature) = find_signature(&name, &args, signatures);

    Expression::call(func, args.into(), signature.return_type)
}

fn find_signature(
    name: &str,
    args: &[Expression],
    signatures: &HashMap<String, FunctionSignature>,
) -> (String, FunctionSignature) {
    let mut mangled = format!("{name}_");

    fn type_name(ty: &Type) -> String {
        match ty {
            Type::List(elem) => format!("list___{}", type_name(elem)),
            Type::Option(some) => format!("option___{}", type_name(some)),
            Type::Result { ok, err } => format!("result___{}___{}", type_name(ok), type_name(err)),
            // TODO: this is a hack to make enums work right now
            Type::Enum { cases: _ } => "s32".to_owned(),
            _ => ty.to_string(),
        }
    }

    let (function_name, signature) = signatures
        .get_key_value(name)
        .or_else(|| {
            for arg in args {
                mangled.push('_');
                mangled.push_str(&type_name(&arg.ty));
            }

            signatures.get_key_value(&mangled)
        })
        .unwrap_or_else(|| panic!("Signature not found for name: {mangled}"));

    (function_name.clone(), signature.clone())
}
