use std::collections::{HashMap, HashSet};
use std::ops::{Deref, Index};

use wtf_ast::{
    self as ast, BinaryOperator, FunctionDeclaration, TestDeclaration, TypeAnnotation,
    UnaryOperator,
};

use crate::builtin::WithBuiltins;
use crate::{
    type_::unify, visible::Visible, Body, Expression, ExpressionKind, Function, FunctionBody,
    FunctionSignature, Module, Parameter, Test, Type, VarId,
};

const INTERNAL_PREFIX: &str = "wtfinternal";
const INTERNAL_SUFFIX: &str = "sdafbvaeiwcoiysxuv";

pub fn compile(ast: ast::Module) -> Module {
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
                panic!("TODO: error: double export is not permitted!")
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
    let mut constants = HashSet::new();
    for (fun, is_export) in ast_funs.values() {
        functions.insert(
            fun.name.to_string(),
            compile_fun(
                fun,
                *is_export,
                &ast_types,
                &types,
                &signatures,
                &mut constants,
            ),
        );
    }

    let mut tests = Vec::new();
    for (idx, test) in ast_tests.into_iter().enumerate() {
        tests.push(compile_test(
            idx,
            test,
            &signatures,
            &types,
            &ast_types,
            &mut constants,
        ));
    }

    Module {
        types,
        functions,
        tests,
        constants,
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
    constants: &mut HashSet<Vec<u8>>,
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
        .unwrap_or(Type::Void);

    let mut vars = VarCollector::new();
    let mut visible = Visible::new();

    for (name, ty) in &parameters {
        let param = vars.push(ty.clone());
        visible.bind(name.clone(), param, false);
    }

    let block = compile_block(&declaration.body, &mut vars, &mut visible, signatures);

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
        .map_or(Type::Void, |ty| compile_type_annotation(&ty, ast_types));

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
    constants: &mut HashSet<Vec<u8>>,
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
        constants,
    );

    Test {
        name: test.name,
        id,
        body: function.body,
    }
}

struct VarCollector {
    vars: Vec<Type>,
}
impl VarCollector {
    fn new() -> Self {
        Self { vars: vec![] }
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
) -> Body {
    let visible_snapshot = visible.snapshot();

    let mut statements = Vec::new();
    for statement in &block.statements {
        if matches!(statement, ast::Statement::EmptyLine) {
            continue;
        }
        statements.push(compile_statement(statement, fun, visible, signatures));
    }

    visible.restore(visible_snapshot);
    Body { statements }
}

fn compile_statement(
    statement: &ast::Statement,
    vars: &mut VarCollector,
    visible: &mut Visible,
    signatures: &HashMap<String, FunctionSignature>,
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
            let ty = initial_value.ty.clone();
            let var = vars.push(ty);
            // TODO: check if type matches annotated type (if exists)
            visible.bind(
                variable_declaration.name.clone(),
                var,
                variable_declaration.mutable,
            );
            Expression::var_set(var, initial_value)
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
            let then = compile_block(&if_statement.then_branch, vars, visible, signatures);
            let else_ = match &if_statement.else_branch {
                Some(else_branch) => compile_block(&else_branch, vars, visible, signatures),
                None => EMPTY_BODY,
            };
            let ty = unify(&then.returns().ty, &else_.returns().ty);
            Expression::if_(condition, then, else_, ty)
        }
        ast::Statement::MatchStatement(_) => todo!("impl match"),
        ast::Statement::WhileStatement(while_statement) => {
            let inner_body = compile_block(&while_statement.body, vars, visible, signatures);
            let complete_body = {
                let mut body = Vec::new();
                let condition =
                    compile_expression(&while_statement.condition, vars, visible, signatures);
                body.push(Expression::if_(
                    condition,
                    EMPTY_BODY,
                    vec![Expression::break_(Expression::void())].into(),
                    Type::Void,
                ));
                for statement in inner_body.statements {
                    body.push(statement);
                }
                body.push(Expression::void());
                body
            };
            Expression::loop_(complete_body.into(), Type::Void)
        }
        ast::Statement::ForStatement(_) => todo!("impl for"),
        wtf_ast::Statement::Assertion(assert_statement) => {
            let condition =
                compile_expression(&assert_statement.condition, vars, visible, signatures);
            Expression::if_(
                condition,
                EMPTY_BODY,
                vec![Expression::unreachable()].into(),
                Type::Void,
            )
        }
    }
}

fn compile_expression(
    expression: &ast::Expression,
    fun: &mut VarCollector,
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
            let binding = visible
                .lookup(&name)
                .expect(&format!("Variable {} is not defined.", name));
            let ty = &fun[binding.id];
            ExpressionKind::VarGet { var: binding.id }.typed(ty.clone())
        }
        ast::Expression::BinaryExpression {
            left,
            operator,
            right,
        } => {
            let left = compile_expression(left, fun, visible, signatures);
            let right = compile_expression(right, fun, visible, signatures);
            // TODO: call "find_signature"
            ExpressionKind::Call {
                function: format!(
                    "{}__{}_{}",
                    match operator {
                        wtf_ast::BinaryOperator::Arithmetic(operator) => match operator {
                            wtf_ast::ArithmeticOperator::Add => "add",
                            wtf_ast::ArithmeticOperator::Subtract => "sub",
                            wtf_ast::ArithmeticOperator::Multiply => "mul",
                            wtf_ast::ArithmeticOperator::Divide => "div",
                        },
                        BinaryOperator::Logic(op) => match op {
                            // TODO: handling this as a function does not allow short-circuiting
                            ast::LogicOperator::And => "and",
                            ast::LogicOperator::Or => "or",
                        },
                        wtf_ast::BinaryOperator::Equal => "eq",
                        wtf_ast::BinaryOperator::NotEqual => "ne",
                        wtf_ast::BinaryOperator::GreaterThan => "greater_than",
                        wtf_ast::BinaryOperator::LessThan => "less_than",
                        wtf_ast::BinaryOperator::GreaterEqual => "greater_eq",
                        wtf_ast::BinaryOperator::LessEqual => "less_eq",
                        wtf_ast::BinaryOperator::Contains => "contains",
                        wtf_ast::BinaryOperator::NullCoalesce => todo!("null coalesce"),
                    },
                    left.ty,
                    right.ty
                ),
                arguments: vec![left.clone(), right],
            }
            .typed(left.ty)
        }
        ast::Expression::UnaryExpression { operator, operand } => {
            let operand = compile_expression(operand, fun, visible, signatures);
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

            let mut args = vec![];
            for arg in arguments {
                args.push(compile_expression(arg, fun, visible, signatures));
            }

            let (function, signature) = find_signature(&function, &args, signatures);

            Expression::call(function.clone(), args, signature.return_type.clone())
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

            let mut args = vec![];
            args.push(compile_expression(receiver, fun, visible, signatures));
            for arg in arguments {
                let arg = compile_expression(arg, fun, visible, signatures);
                args.push(arg);
            }

            let (method, signature) = find_signature(&method, &args, signatures);

            Expression::call(method.clone(), args, signature.return_type)
        }
        ast::Expression::FieldAccess {
            object,
            field,
            // TODO: Desugar safe calls to if condition
            safe,
        } => {
            if *safe {
                todo!("Safe calls");
            }
            let object = compile_expression(object, fun, visible, signatures);
            let object_ty = object.ty.clone();
            let member_type = match object_ty {
                Type::Never => Type::Never,
                Type::Record(fields) => fields
                    .get(field)
                    .expect("No field named {field} on {ty}.")
                    .clone(),
                _ => panic!("field access on non-struct"),
            };
            ExpressionKind::Member {
                of: object.into(),
                name: field.clone(),
            }
            .typed(member_type)
        }
        ast::Expression::IndexAccess { collection, index } => {
            let collection = compile_expression(&collection, fun, visible, signatures);
            let index = compile_expression(index, fun, visible, signatures);
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
                let value = compile_expression(&member.element, fun, visible, signatures);
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
                let item = compile_expression(item, fun, visible, signatures);
                compiled_items.push(item);
            }
            let item_ty = if compiled_items.is_empty() {
                Type::Never
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

    for (actual, expected) in args.iter().zip(signature.param_types.iter()) {
        let arg = &actual.ty;
        if arg != expected {
            // TODO: better error reporting, similar to parser
            todo!(
                "Compiler error: argument type does not match for {function_name}.\nGot: {arg}, Expected: {expected}",
            )
        }
    }

    (function_name.clone(), signature.clone())
}
