use std::collections::HashMap;

use wtf_ast::{self as ast, ResourceDeclaration};

use crate::{Module, ResourceType, Type};

fn compile(ast: ast::Module) -> Module {
    Module {
        types: compile_types(ast),
        functions: HashMap::new(),
    }
}

fn compile_types(ast: ast::Module) -> HashMap<String, Type> {
    let mut ast_types = HashMap::new();
    for declaration in ast.declarations {
        if let Some(name) = type_name(declaration) {
            ast_types.insert(name, declaration);
        }
    }

    let mut types = HashMap::new();
    for decl in ast_types.values() {
        compile_type_declaration(decl, &ast_types, &mut types);
    }

    println!("{:?}", types);

    types
}
fn type_name(declaration: ast::Declaration) -> Option<String> {
    match declaration {
        ast::Declaration::Function(_) => None,
        ast::Declaration::Record(decl) => Some(decl.name),
        ast::Declaration::Resource(decl) => Some(decl.name),
        ast::Declaration::Enum(decl) => Some(decl.name),
        ast::Declaration::Variant(decl) => Some(decl.name),
        ast::Declaration::Export(_) => None,
        ast::Declaration::Package(_) => None,
        ast::Declaration::Use(_) => None,
    }
}
fn compile_type_declaration(
    declaration: ast::Declaration,
    ast_types: &HashMap<String, ast::Declaration>,
    out: &mut HashMap<String, Type>,
) -> Type {
    let name = type_name(declaration).expect("not a type definition");
    if let Some(type_) = out.get(&name) {
        return type_.clone();
    }
    let type_ = match declaration {
        ast::Declaration::Record(record) => {
            let mut fields = HashMap::new();
            for field in record.fields {
                fields.insert(
                    field.name,
                    compile_type_annotation(field.type_annotation, ast_types, out),
                );
            }
            Type::Record(fields)
        }
        ast::Declaration::Resource(resource) => {
            let mut methods = HashMap::new();
            for method in resource.methods {
                methods.insert(
                    method.name,
                    FunctionSignature {
                        methods: method.parameters.iter().map(|param| {
                            compile_type_annotation(param.type_annotation, ast_types, out)
                        }),
                        return_type: compile_type_annotation(method.return_type, ast_types, out),
                    },
                );
            }
            Type::Resource(ResourceType { methods })
        }
        ast::Declaration::Enum(decl) => Some(decl.name),
        ast::Declaration::Variant(decl) => Some(decl.name),
        _ => unreachable!(),
    };
    out.insert(type_);
    type_
}
fn compile_type_annotation(
    annotation: ast::TypeAnnotation,
    ast_types: &HashMap<String, ast::Declaration>,
    out: &mut HashMap<String, Type>,
) -> Type {
    match annotation {
        ast::TypeAnnotation::Simple(name) => {
            let declaration = ast_types.get(name).expect("unknown type");
            compile_type_declaration(declaration, ast_types, out)
        }
        ast::TypeAnnotation::List(item) => {
            Type::List(compile_type_annotation(item, ast_types, out))
        }
        ast::TypeAnnotation::Option(payload) => {
            Type::Option(compile_type_annotation(payload, ast_types, out))
        }
        ast::TypeAnnotation::Result { ok, err } => Type::Result {
            ok: compile_type_annotation(ok, ast_types, out),
            err: compile_type_annotation(err, ast_types, out),
        },
        ast::TypeAnnotation::Tuple(fields) => Type::Tuple(
            fields
                .into_iter()
                .map(|field| compile_type_annotation(field, ast_types, out))
                .collect_vec(),
        ),
    }
}
