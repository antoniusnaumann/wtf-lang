use tower_lsp::lsp_types::*;
use wtf_ast;
use wtf_hir;

use crate::Backend;

impl Backend {
    /// HIR-based type inference that uses actual type checking results
    pub async fn infer_expression_type_hir(&self, expression: &str, uri: &Url) -> Option<String> {
        // First, try to get the HIR from cache
        let hir_cache = self.hir_cache.read().await;
        if let Some(hir) = hir_cache.get(uri) {
            // Look for the expression in the HIR variable names
            if let Some(type_name) = self.find_variable_type_in_hir(hir, expression, uri).await {
                return Some(type_name);
            }
        }
        
        // Fallback to AST-based inference for now
        self.infer_expression_type_ast(expression, uri).await
    }

    async fn find_variable_type_in_hir(&self, hir: &wtf_hir::Module, variable_name: &str, uri: &Url) -> Option<String> {
        // Search through all functions to find a variable with the given name
        for (_, function) in &hir.functions {
            // Check function parameters first
            for param in &function.parameters {
                if param.name == variable_name {
                    return Some(self.type_to_string(&param.ty, uri).await);
                }
            }
            
            // Check local variables using the var_names we added
            for (i, var_name_opt) in function.body.var_names.iter().enumerate() {
                if let Some(var_name) = var_name_opt {
                    if var_name == variable_name {
                        if let Some(var_type) = function.body.vars.get(i) {
                            return Some(self.type_to_string(var_type, uri).await);
                        }
                    }
                }
            }
        }
        
        None
    }

    async fn type_to_string(&self, hir_type: &wtf_hir::Type, uri: &Url) -> String {
        match hir_type {
            wtf_hir::Type::Record(fields) => {
                // Try to map the HIR record back to an AST record name
                if let Some(record_name) = self.find_matching_record_name(fields, uri).await {
                    record_name
                } else {
                    // Fallback to generic name
                    format!("Record_{}", fields.len())
                }
            },
            wtf_hir::Type::Resource { .. } => "Resource".to_string(),
            wtf_hir::Type::String => "String".to_string(),
            wtf_hir::Type::Int { signed, bits } => {
                format!("{}{}", if *signed { "i" } else { "u" }, bits)
            },
            wtf_hir::Type::Float { bits } => format!("f{}", bits),
            wtf_hir::Type::Bool => "Bool".to_string(),
            wtf_hir::Type::List(inner) => format!("List_{}", self.type_to_string_sync(inner)),
            wtf_hir::Type::Option(inner) => format!("Option_{}", self.type_to_string_sync(inner)),
            _ => "Unknown".to_string(),
        }
    }

    // Synchronous version for recursive calls
    fn type_to_string_sync(&self, hir_type: &wtf_hir::Type) -> String {
        match hir_type {
            wtf_hir::Type::Record(fields) => format!("Record_{}", fields.len()),
            wtf_hir::Type::Resource { .. } => "Resource".to_string(),
            wtf_hir::Type::String => "String".to_string(),
            wtf_hir::Type::Int { signed, bits } => {
                format!("{}{}", if *signed { "i" } else { "u" }, bits)
            },
            wtf_hir::Type::Float { bits } => format!("f{}", bits),
            wtf_hir::Type::Bool => "Bool".to_string(),
            wtf_hir::Type::List(inner) => format!("List_{}", self.type_to_string_sync(inner)),
            wtf_hir::Type::Option(inner) => format!("Option_{}", self.type_to_string_sync(inner)),
            _ => "Unknown".to_string(),
        }
    }

    async fn find_matching_record_name(&self, hir_fields: &[(String, wtf_hir::Type)], uri: &Url) -> Option<String> {
        let ast_cache = self.ast_cache.read().await;
        let ast = ast_cache.get(uri)?;
        
        // Extract just the field names from HIR
        let hir_field_names: Vec<&String> = hir_fields.iter().map(|(name, _)| name).collect();
        
        // Find a record in AST that has the same field names
        for declaration in &ast.declarations {
            if let wtf_ast::Declaration::Record(record) = declaration {
                let ast_field_names: Vec<&String> = record.fields.iter().map(|f| &f.name).collect();
                
                // Check if field names match exactly
                if hir_field_names.len() == ast_field_names.len() && 
                   hir_field_names.iter().all(|name| ast_field_names.contains(name)) {
                    return Some(record.name.clone());
                }
            }
        }
        
        None
    }

    /// AST-based type inference (improved version of the original)
    pub async fn infer_expression_type_ast(&self, expression: &str, uri: &Url) -> Option<String> {
        let ast_cache = self.ast_cache.read().await;
        let ast = ast_cache.get(uri)?;
        
        // Try to find the identifier in record literals first
        if let Some(record_type) = self.infer_from_record_literal(ast, expression) {
            return Some(record_type);
        }
        
        // Try to find the identifier in function parameters and variable declarations
        if let Some(type_name) = self.find_identifier_type(ast, expression) {
            return Some(type_name);
        }
        
        None
    }

    fn infer_from_record_literal(&self, ast: &wtf_ast::Module, identifier: &str) -> Option<String> {
        // Look for record literals that assign to this identifier
        for declaration in &ast.declarations {
            if let Some(type_name) = self.find_record_assignment(declaration, identifier) {
                return Some(type_name);
            }
        }
        None
    }

    fn find_record_assignment(&self, declaration: &wtf_ast::Declaration, identifier: &str) -> Option<String> {
        match declaration {
            wtf_ast::Declaration::Function(func) => {
                self.find_record_assignment_in_block(&func.body, identifier)
            },
            wtf_ast::Declaration::Test(test) => {
                self.find_record_assignment_in_block(&test.body, identifier)
            },
            _ => None,
        }
    }

    fn find_record_assignment_in_block(&self, block: &wtf_ast::Block, identifier: &str) -> Option<String> {
        for statement in &block.statements {
            if let Some(type_name) = self.find_record_assignment_in_statement(statement, identifier) {
                return Some(type_name);
            }
        }
        None
    }

    fn find_record_assignment_in_statement(&self, statement: &wtf_ast::Statement, identifier: &str) -> Option<String> {
        match statement {
            wtf_ast::Statement::VariableDeclaration(var_decl) => {
                if var_decl.name == identifier {
                    if let Some(value) = &var_decl.value {
                        return self.extract_record_type_from_expression(value);
                    }
                }
            },
            wtf_ast::Statement::Assignment { target, value } => {
                if let wtf_ast::ExpressionKind::Identifier(name) = &target.kind {
                    if name == identifier {
                        return self.extract_record_type_from_expression(value);
                    }
                }
            },
            wtf_ast::Statement::IfStatement(if_stmt) => {
                if let Some(type_name) = self.find_record_assignment_in_block(&if_stmt.then_branch, identifier) {
                    return Some(type_name);
                }
                if let Some(else_block) = &if_stmt.else_branch {
                    if let Some(type_name) = self.find_record_assignment_in_block(else_block, identifier) {
                        return Some(type_name);
                    }
                }
            },
            _ => {},
        }
        None
    }

    fn extract_record_type_from_expression(&self, expression: &wtf_ast::Expression) -> Option<String> {
        match &expression.kind {
            wtf_ast::ExpressionKind::Record { members, .. } => {
                // Extract field names to create a type signature
                let field_names: Vec<String> = members.iter()
                    .map(|field| field.name.clone())
                    .collect();
                
                // Use field count and first few field names for type identification
                Some(format!("Record_{}_{}", field_names.len(), field_names.get(0).unwrap_or(&"unknown".to_string())))
            },
            _ => None,
        }
    }

    fn find_identifier_type(&self, ast: &wtf_ast::Module, identifier: &str) -> Option<String> {
        // Check function parameters and local variables
        for declaration in &ast.declarations {
            if let Some(type_name) = self.find_identifier_in_declaration(declaration, identifier) {
                return Some(type_name);
            }
        }
        None
    }

    fn find_identifier_in_declaration(&self, declaration: &wtf_ast::Declaration, identifier: &str) -> Option<String> {
        match declaration {
            wtf_ast::Declaration::Function(func) => {
                // Check parameters
                for param in &func.parameters {
                    if param.name == identifier {
                        return self.type_annotation_to_string(&param.type_annotation);
                    }
                }
                // Check function body for variable declarations
                self.find_identifier_in_block(&func.body, identifier)
            },
            wtf_ast::Declaration::Test(test) => {
                self.find_identifier_in_block(&test.body, identifier)
            },
            _ => None,
        }
    }

    fn find_identifier_in_block(&self, block: &wtf_ast::Block, identifier: &str) -> Option<String> {
        for statement in &block.statements {
            if let Some(type_name) = self.find_identifier_in_statement(statement, identifier) {
                return Some(type_name);
            }
        }
        None
    }

    fn find_identifier_in_statement(&self, statement: &wtf_ast::Statement, identifier: &str) -> Option<String> {
        match statement {
            wtf_ast::Statement::VariableDeclaration(var_decl) => {
                if var_decl.name == identifier {
                    if let Some(type_annotation) = &var_decl.type_annotation {
                        return self.type_annotation_to_string(type_annotation);
                    }
                }
            },
            _ => {},
        }
        None
    }

    fn type_annotation_to_string(&self, type_annotation: &wtf_ast::TypeAnnotation) -> Option<String> {
        match &type_annotation.kind {
            wtf_ast::TypeAnnotationKind::Simple(name) => Some(name.clone()),
            _ => None,
        }
    }
}