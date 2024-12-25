use std::{
    collections::{HashMap, HashSet},
    ops::Deref,
};

use wtf_ast::{self as ast, BinaryOperator, Expression, TypeAnnotation};

use crate::{
    builtin::WithBuiltins, visible::Visible, Block, Function, FunctionSignature, Instruction,
    LocalId, Module, PrimitiveType, ResourceType, Type,
};

pub fn compile(ast: ast::Module) -> Module {
    // TODO: Convert into lookup of name -> export? on first pass
    let mut ast_types = HashMap::new();
    let mut ast_funs = HashMap::new();
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
        }
    }

    let mut types = HashMap::with_builtins();
    for (ty, is_export) in ast_types.values() {
        types.insert(
            ty.name().to_owned(),
            compile_type_declaration(ty, *is_export, &ast_types),
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

    let mut constants = HashSet::new();
    let mut functions = HashMap::new();
    for (fun, is_export) in ast_funs.values() {
        functions.insert(
            fun.name.to_string(),
            compile_fun(fun, *is_export, &ast_types, &signatures, &mut constants),
        );
    }

    Module {
        types,
        functions,
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
    ast_types: &HashMap<String, (ast::Declaration, bool)>,
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
            "char" => PrimitiveType::Char,
            "string" => PrimitiveType::String,
            _ => {
                let (declaration, is_export) = ast_types
                    .get(name)
                    .unwrap_or_else(|| panic!("unknown type {name}"));
                return compile_type_declaration(declaration, *is_export, ast_types);
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
    is_export: bool,
    ast_types: &HashMap<String, (ast::Declaration, bool)>,
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
        .unwrap_or(Type::None);

    let mut fn_compiler = FunctionCompiler::with_params(&parameters, signatures, constants);

    let body = fn_compiler.compile_block(&declaration.body);
    Function {
        parameters,
        return_type,
        locals: fn_compiler.locals,
        body,
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

struct FunctionCompiler<'a> {
    visible: Visible,
    stack: Vec<Type>,
    locals: Vec<Type>,

    signatures: &'a HashMap<String, FunctionSignature>,
    constants: &'a mut HashSet<Vec<u8>>,
}

impl<'a> FunctionCompiler<'a> {
    fn with_params(
        parameters: &[(String, Type)],
        signatures: &'a HashMap<String, FunctionSignature>,
        constants: &'a mut HashSet<Vec<u8>>,
    ) -> Self {
        let param_types: Vec<_> = parameters.iter().map(|(_, ty)| ty.clone()).collect();
        let mut visible = Visible::new();
        for (i, (s, _)) in parameters.iter().enumerate() {
            visible.bind(s.clone(), LocalId(i), false);
        }

        FunctionCompiler {
            visible,
            stack: param_types.clone(),
            locals: param_types,
            signatures,
            constants,
        }
    }

    fn push(&mut self, instruction: Instruction, block: &mut Block) {
        self.apply(&instruction);
        block.instructions.push(instruction);
    }

    fn apply(&mut self, instruction: &Instruction) {
        match instruction {
            Instruction::Pop => {
                self.stack.pop();
            }
            Instruction::Load(local_id) => {
                self.stack.push(self.locals[local_id.0].clone());
            }
            Instruction::Store(local_id) => {
                self.stack.pop(); // TODO: ensure types match
            }
            Instruction::Int(_) => self.stack.push(Type::Builtin(PrimitiveType::S64)),
            Instruction::Float(_) => self.stack.push(Type::Builtin(PrimitiveType::F64)),
            Instruction::Bool(_) => self.stack.push(Type::Builtin(PrimitiveType::Bool)),
            Instruction::String(_) => self.stack.push(Type::Builtin(PrimitiveType::String)),
            Instruction::None => self.stack.push(Type::None),
            Instruction::Enum {
                variant,
                num_payloads,
            } => {
                let mut payloads = vec![];
                for _ in 0..*num_payloads {
                    payloads.push(self.stack.pop().unwrap());
                }
                let mut type_ = HashMap::new();
                type_.insert(variant.to_string(), payloads);
                self.stack.push(Type::Variant(todo!()));
            }
            Instruction::Record(field_names) => {
                let mut fields = HashMap::new();
                for field in field_names {
                    fields.insert(field.clone(), self.stack.pop().unwrap());
                }
                self.stack.push(Type::Record(fields));
            }
            Instruction::List(num_items) => {
                let mut items = vec![];
                for _ in 0..*num_items {
                    items.push(self.stack.pop().unwrap());
                }
                self.stack.push(Type::List(Box::new(items[0].clone()))); // TODO: ensure items have same type
            }
            Instruction::Call {
                function,
                num_arguments,
            } => {
                let mut args = vec![];
                for _ in 0..*num_arguments {
                    args.push(self.stack.pop().unwrap());
                }
                // The names here should already be resolved to their fully qualified name
                self.stack.push(
                    self.signatures
                        .get(function)
                        .expect(&format!("Resolved function {function} should exist"))
                        .return_type
                        .clone(),
                );
            }
            Instruction::MemberChain(id, fields) => {
                let mut ty = &self.locals[id.0];
                for field in fields {
                    let Type::Record(fields) = ty else {
                        panic!("Member access on non-record.")
                    };

                    ty = &fields[field];
                }

                self.stack.push(ty.clone());
            }
            Instruction::FieldAccess(field) => {
                if let Type::Record(record) = self.stack.pop().unwrap() {
                    self.stack.push(record[field].clone());
                } else {
                    panic!("Field access on non-record.");
                }
            }
            Instruction::IndexAccess => {
                if let Type::List(list) = self.stack.pop().unwrap() {
                    self.stack.push(*list);
                } else {
                    panic!("Index access of non-list.");
                }
            }
            Instruction::Return => {
                self.stack.push(Type::Never);
            }
            Instruction::Break => {
                todo!("divert")
            }
            Instruction::Continue => {
                todo!("divert")
            }
            Instruction::Throw => {
                todo!("divert")
            }
            Instruction::If { then, else_ } => {
                let condition = self.stack.pop().unwrap();
                assert!(
                    condition == Type::Builtin(PrimitiveType::Bool),
                    "Condition: {:#?}",
                    condition
                );
            }
            Instruction::Match { arms } => {
                let condition = self.stack.pop().unwrap();
            }
            Instruction::Loop(block) => todo!(),
            Instruction::Unreachable => self.stack.push(Type::Never),
        }
    }

    fn compile_block(&mut self, block: &ast::Block) -> Block {
        let restore = self.stack.clone();
        let mut result = Block {
            instructions: Vec::new(),
            ty: Type::None,
        };
        for statement in &block.statements {
            self.compile_statement(statement, &mut result);
        }
        if result.instructions.is_empty() {
            result.instructions.push(Instruction::None);
        }
        result.ty = self.stack.pop().unwrap_or(Type::None);
        self.stack = restore;
        result
    }

    fn compile_statement(&mut self, statement: &ast::Statement, block: &mut Block) {
        match statement {
            ast::Statement::VariableDeclaration(variable_declaration) => {
                // TODO: allow uninitialized variables
                self.compile_expression(variable_declaration.value.as_ref().unwrap(), block);
                let type_ = self.stack.last().unwrap().clone();
                let local = LocalId(self.locals.len());
                self.locals.push(type_);
                self.visible.bind(
                    variable_declaration.name.clone(),
                    local,
                    variable_declaration.mutable,
                );
                self.push(Instruction::Store(local), block);
            }
            ast::Statement::Assignment { target, value } => {
                self.compile_expression(value, block);
                if let ast::Expression::Identifier(name) = target {
                    let local = self.visible.lookup(name).expect("Name {name} not defined");
                    self.push(Instruction::Store(local), block);
                } else {
                    panic!("Can only assign to names");
                }
            }
            ast::Statement::ExpressionStatement(expression) => {
                self.compile_expression(expression, block);
                self.push(Instruction::Pop, block);
            }
            ast::Statement::ReturnStatement(expression) => {
                match expression {
                    Some(expression) => self.compile_expression(expression, block),
                    None => self.push(Instruction::None, block),
                }
                self.push(Instruction::Return, block);
            }
            ast::Statement::BreakStatement(expression) => {
                match expression {
                    Some(expression) => self.compile_expression(expression, block),
                    None => self.push(Instruction::None, block),
                };
                self.push(Instruction::Break, block)
            }
            ast::Statement::ContinueStatement => self.push(Instruction::Continue, block),
            ast::Statement::ThrowStatement(throw) => {
                self.compile_expression(throw, block);
                self.push(Instruction::Throw, block);
            }
            ast::Statement::IfStatement(if_statement) => {
                self.compile_expression(&if_statement.condition, block);
                let then = self.compile_block(&if_statement.then_branch);
                let else_ = match &if_statement.else_branch {
                    Some(else_branch) => self.compile_block(&else_branch),
                    None => Block::new(),
                };
                let after = match (&then.ty, &else_.ty) {
                    (Type::Never, Type::Never) => vec![Instruction::Unreachable],
                    (Type::Never, _) => vec![],
                    (_, Type::Never) => vec![],
                    (a, b) if a == b => vec![],
                    _ => todo!("If and else arm types must match or diverge!"),
                };
                self.push(Instruction::If { then, else_ }, block);
                for instruction in after {
                    self.push(instruction, block);
                }
            }
            ast::Statement::MatchStatement(_) => todo!(),
            ast::Statement::WhileStatement(while_statement) => {
                let mut inner_body = Block::new();
                self.compile_expression(&while_statement.condition, block);
                let totally_inner_body = self.compile_block(&while_statement.body);
                self.push(
                    Instruction::If {
                        then: totally_inner_body,
                        else_: Block {
                            instructions: vec![Instruction::None, Instruction::Break],
                            ty: Type::Never,
                        },
                    },
                    &mut inner_body,
                );
                self.push(Instruction::Loop(inner_body), block)
            }
            ast::Statement::ForStatement(_) => todo!(),
        }
    }

    fn compile_expression(&mut self, expression: &ast::Expression, block: &mut Block) {
        match expression {
            ast::Expression::Literal(literal) => {
                let lit = match literal {
                    ast::Literal::Integer(int) => Instruction::Int(*int),
                    ast::Literal::Float(float) => Instruction::Float(*float),
                    ast::Literal::String(string) => {
                        self.constants.insert(string.as_bytes().into());
                        Instruction::String(string.clone())
                    }
                    ast::Literal::Boolean(bool) => Instruction::Bool(*bool),
                    ast::Literal::None => Instruction::None,
                };

                self.push(lit, block)
            }
            ast::Expression::Identifier(name) => {
                let local = self
                    .visible
                    .lookup(&name)
                    .expect(&format!("Variable {} is not defined.", name));
                self.push(Instruction::Load(local), block);
            }
            ast::Expression::BinaryExpression {
                left,
                operator,
                right,
            } => {
                self.push_op(left, right, *operator, block);
            }
            ast::Expression::UnaryExpression { .. } => todo!(),
            ast::Expression::YeetExpression { .. } => todo!(),
            ast::Expression::FunctionCall {
                function,
                arguments,
            } => {
                self.push_fn(function, arguments, block);
            }
            ast::Expression::MethodCall { .. } => todo!(),
            ast::Expression::FieldAccess {
                object,
                field,
                // TODO: Desugar safe calls to if condition
                safe,
            } => {
                let mut inner = object;
                let mut fields = vec![field];
                // Find out if this is a chain of member fields on a local value
                loop {
                    match inner.deref() {
                        Expression::FieldAccess {
                            object,
                            field,
                            safe: false,
                        } => {
                            inner = object;
                            fields.push(field);
                        }
                        Expression::Identifier(name) => {
                            // TODO: find ident here
                            let local = self
                                .visible
                                .lookup(&name)
                                .expect(&format!("Variable {} is not defined.", name));
                            self.push(
                                Instruction::MemberChain(
                                    local,
                                    fields.into_iter().cloned().rev().collect(),
                                ),
                                block,
                            );
                            break;
                        }
                        _ => {
                            self.compile_expression(&object, block);

                            self.push(Instruction::FieldAccess(field.to_string()), block);
                            break;
                        }
                    }
                }
            }
            ast::Expression::IndexAccess { collection, index } => {
                self.compile_expression(&collection, block);
                self.compile_expression(index, block);
                self.push(Instruction::IndexAccess, block);
            }
            ast::Expression::Record { name, members } => {
                let mut fields = vec![];
                for member in members {
                    self.compile_expression(&member.element, block);
                    fields.push(member.name.to_string());
                }
                self.push(Instruction::Record(fields), block);
            }
            ast::Expression::ListLiteral(items) => {
                let num_items = items.len();
                for item in items {
                    self.compile_expression(item, block);
                }
                self.push(Instruction::List(num_items), block);
            }
        }
    }

    fn push_fn(&mut self, function: &Expression, arguments: &[Expression], block: &mut Block) {
        fn mangle(ty: &Type) -> std::borrow::Cow<str> {
            match ty {
                Type::Never => panic!("Never as an arg is not allowed"),
                Type::None => panic!("None as an arg ist not allowed"),
                Type::List(elem) => format!("list___{}", mangle(elem)).into(),
                Type::Option(inner) => format!("option___{}", mangle(inner)).into(),
                Type::Result { ok, err } => {
                    format!("result___{}__{}", mangle(ok), mangle(err)).into()
                }
                Type::Record(_) => todo!(),
                Type::Resource(_) => todo!(),
                Type::Enum(_) => todo!(),
                Type::Variant(_) => todo!(),
                Type::Tuple(_) => todo!(),
                Type::Builtin(ty) => ty.name().into(),
            }
        }

        fn mangle_fn<'a>(name: &'a str, args: &[Type]) -> std::borrow::Cow<'a, str> {
            if args.len() == 0 {
                name.into()
            } else {
                let postfix = args
                    .iter()
                    .map(|arg| mangle(arg))
                    .collect::<Vec<_>>()
                    .join("_");
                format!("{name}__{postfix}").into()
            }
        }

        let mut arg_types = vec![];
        for arg in arguments {
            self.compile_expression(arg, block);
            arg_types.push(self.stack.last().unwrap().clone());
        }

        // TODO: Function overloading for non-builtin functions
        // TODO: Resolve functions into fully qualified names
        let function = match function {
            ast::Expression::Identifier(name) if self.signatures.contains_key(name) => name.into(),
            ast::Expression::Identifier(name) => {
                let mangled = mangle_fn(name, &arg_types);
                if self.signatures.contains_key(mangled.deref()) {
                    mangled.into()
                } else {
                    dbg!(&self.signatures.keys());
                    panic!("Function '{name}' does not exist");
                }
            }
            _ => panic!("You can only call names."),
        };
        self.push(
            Instruction::Call {
                function,
                num_arguments: arguments.len(),
            },
            block,
        );
    }

    fn push_op(
        &mut self,
        left: &Expression,
        right: &Expression,
        op: BinaryOperator,
        block: &mut Block,
    ) {
        let name = match op {
            BinaryOperator::Arithmetic(op) => match op {
                ast::ArithmeticOperator::Add => "add",
                ast::ArithmeticOperator::Subtract => "sub",
                ast::ArithmeticOperator::Multiply => "mul",
                ast::ArithmeticOperator::Divide => "div",
            },
            BinaryOperator::Equal => "eq",
            BinaryOperator::NotEqual => "ne",
            BinaryOperator::GreaterThan => "greater_eq",
            BinaryOperator::LessThan => "less_than",
            BinaryOperator::GreaterEqual => "greater_eq",
            BinaryOperator::LessEqual => "less_eq",
            BinaryOperator::Contains => todo!(),
            BinaryOperator::NullCoalesce => todo!(),
        };
        // TODO: Append argument types from inferred expression types
        let typed_name = format!("{name}__s64_s64");
        let ident = Expression::Identifier(typed_name.into());

        // TODO: Avoid cloning
        self.push_fn(&ident, &[left.clone(), right.clone()], block)
    }
}
