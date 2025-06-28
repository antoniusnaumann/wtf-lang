use tower_lsp::lsp_types::*;
use wtf_ast;

use crate::Backend;

impl Backend {
    pub async fn provide_completion(&self, params: CompletionParams) -> Option<CompletionResponse> {
        let uri = &params.text_document_position.text_document.uri;
        let position = params.text_document_position.position;
        
        let documents = self.documents.read().await;
        let text = documents.get(uri)?;
        
        // Check if we're after a dot (field/method completion)
        if let Some(completions) = self.get_dot_completions(text, position, uri).await {
            return Some(CompletionResponse::Array(completions));
        }
        
        // Check for general identifier completion
        if let Some(completions) = self.get_identifier_completions(text, position, uri).await {
            return Some(CompletionResponse::Array(completions));
        }
        
        None
    }

    async fn get_dot_completions(&self, text: &str, position: Position, uri: &Url) -> Option<Vec<CompletionItem>> {
        let chars: Vec<char> = text.chars().collect();
        let line_start = self.find_line_start(position.line, &chars);
        let line_end = self.find_line_end(position.line, &chars);
        
        if line_end <= line_start {
            return None;
        }
        
        let line_text: String = chars[line_start..line_end].iter().collect();
        let cursor_pos = position.character as usize;
        
        if cursor_pos == 0 || cursor_pos > line_text.len() {
            return None;
        }
        
        // Check if the character before cursor is a dot
        let text_before_cursor = &line_text[..cursor_pos.min(line_text.len())];
        if !text_before_cursor.ends_with('.') {
            return None;
        }
        
        // Extract the expression before the dot
        let before_dot = &text_before_cursor[..text_before_cursor.len() - 1];
        if let Some(word_start) = before_dot.rfind(char::is_whitespace) {
            let identifier = before_dot[word_start..].trim();
            
            // Try to infer the type using HIR-based type inference
            if let Some(type_name) = self.infer_expression_type_hir(identifier, uri).await {
                let ast_cache = self.ast_cache.read().await;
                if let Some(ast) = ast_cache.get(uri) {
                    let completions = self.get_type_completions(ast, &type_name);
                    drop(ast_cache);
                    return Some(completions);
                }
            }
        }
        
        None
    }

    fn get_type_completions(&self, ast: &wtf_ast::Module, type_name: &str) -> Vec<CompletionItem> {
        let mut completions = Vec::new();
        
        // Check if this is an anonymous record type
        if type_name.starts_with("AnonymousRecord:") {
            // Extract field names from the anonymous record type identifier
            if let Some(fields_part) = type_name.strip_prefix("AnonymousRecord:") {
                if !fields_part.is_empty() {
                    let field_names: Vec<&str> = fields_part.split(',').collect();
                    for field in field_names {
                        completions.push(CompletionItem {
                            label: field.to_string(),
                            kind: Some(CompletionItemKind::FIELD),
                            detail: Some("Field of anonymous record".to_string()),
                            ..Default::default()
                        });
                    }
                }
            }
            // Don't return early - continue to add UFCS methods for anonymous types too
        } else {
            // 1. Get field completions for declared record types
            let field_names = self.get_type_fields(ast, type_name);
            for field in field_names {
                completions.push(CompletionItem {
                    label: field.clone(),
                    kind: Some(CompletionItemKind::FIELD),
                    detail: Some(format!("Field of {}", type_name)),
                    ..Default::default()
                });
            }
        }
        
        // 2. Get method completions for resource types
        let methods = self.get_type_methods(ast, type_name);
        for (method_name, signature) in methods {
            completions.push(CompletionItem {
                label: method_name.clone(),
                kind: Some(CompletionItemKind::METHOD),
                detail: Some(signature),
                insert_text: Some(format!("{}()", method_name)),
                ..Default::default()
            });
        }
        
        // 3. Add UFCS method completions
        let ufcs_methods = self.get_ufcs_methods(ast, type_name);
        for (method_name, signature) in ufcs_methods {
            completions.push(CompletionItem {
                label: method_name.clone(),
                kind: Some(CompletionItemKind::FUNCTION),
                detail: Some(format!("UFCS: {}", signature)),
                insert_text: Some(format!("{}()", method_name)),
                ..Default::default()
            });
        }
        
        completions
    }

    async fn get_identifier_completions(&self, text: &str, position: Position, uri: &Url) -> Option<Vec<CompletionItem>> {
        let chars: Vec<char> = text.chars().collect();
        let line_start = self.find_line_start(position.line, &chars);
        let line_end = self.find_line_end(position.line, &chars);
        
        if line_end <= line_start {
            return None;
        }
        
        let line_text: String = chars[line_start..line_end].iter().collect();
        let cursor_pos = position.character as usize;
        
        if cursor_pos > line_text.len() {
            return None;
        }
        
        // Check if we're in a context where identifiers are valid
        // Skip if we're after a dot (that's handled by dot completions)
        let text_before_cursor = &line_text[..cursor_pos.min(line_text.len())];
        if text_before_cursor.ends_with('.') {
            return None;
        }
        
        // Check if we're typing an identifier (partial word or at start of word position)
        let should_complete = self.is_identifier_completion_context(text_before_cursor);
        if !should_complete {
            return None;
        }
        
        let ast_cache = self.ast_cache.read().await;
        let ast = ast_cache.get(uri)?;
        
        let mut completions = Vec::new();
        
        // Add function names
        for declaration in &ast.declarations {
            if let wtf_ast::Declaration::Function(func) = declaration {
                completions.push(CompletionItem {
                    label: func.name.clone(),
                    kind: Some(CompletionItemKind::FUNCTION),
                    detail: Some(self.format_function_signature(func)),
                    insert_text: Some(format!("{}()", func.name)),
                    ..Default::default()
                });
            }
        }
        
        // Add type names (records, resources, enums, variants)
        for declaration in &ast.declarations {
            match declaration {
                wtf_ast::Declaration::Record(record) => {
                    completions.push(CompletionItem {
                        label: record.name.clone(),
                        kind: Some(CompletionItemKind::STRUCT),
                        detail: Some("Record type".to_string()),
                        ..Default::default()
                    });
                },
                wtf_ast::Declaration::Resource(resource) => {
                    completions.push(CompletionItem {
                        label: resource.name.clone(),
                        kind: Some(CompletionItemKind::CLASS),
                        detail: Some("Resource type".to_string()),
                        ..Default::default()
                    });
                },
                wtf_ast::Declaration::Enum(enum_decl) => {
                    completions.push(CompletionItem {
                        label: enum_decl.name.clone(),
                        kind: Some(CompletionItemKind::ENUM),
                        detail: Some("Enum type".to_string()),
                        ..Default::default()
                    });
                },
                wtf_ast::Declaration::Variant(variant) => {
                    completions.push(CompletionItem {
                        label: variant.name.clone(),
                        kind: Some(CompletionItemKind::ENUM),
                        detail: Some("Variant type".to_string()),
                        ..Default::default()
                    });
                },
                _ => {}
            }
        }
        
        // Add variables from HIR if available (parameters, local variables)
        if let Some(vars) = self.get_variables_in_scope(position, uri).await {
            for (var_name, var_type) in vars {
                completions.push(CompletionItem {
                    label: var_name,
                    kind: Some(CompletionItemKind::VARIABLE),
                    detail: Some(format!("Variable: {}", var_type)),
                    ..Default::default()
                });
            }
        }
        
        if completions.is_empty() {
            None
        } else {
            Some(completions)
        }
    }

    fn is_identifier_completion_context(&self, text_before_cursor: &str) -> bool {
        let trimmed = text_before_cursor.trim_end();
        
        // If empty or ends with whitespace, we're likely at start of new identifier
        if trimmed.is_empty() || text_before_cursor.ends_with(char::is_whitespace) {
            return true;
        }
        
        // If we're in the middle of typing an identifier
        if let Some(last_word_start) = trimmed.rfind(char::is_whitespace) {
            let last_word = &trimmed[last_word_start..].trim_start();
            // Check if it looks like a partial identifier (alphanumeric + underscore)
            return last_word.chars().all(|c| c.is_alphanumeric() || c == '_') && !last_word.is_empty();
        }
        
        // If the whole line is a potential identifier
        trimmed.chars().all(|c| c.is_alphanumeric() || c == '_') && !trimmed.is_empty()
    }

    async fn get_variables_in_scope(&self, position: Position, uri: &Url) -> Option<Vec<(String, String)>> {
        let hir_cache = self.hir_cache.read().await;
        let hir = hir_cache.get(uri)?;
        
        let mut variables = Vec::new();
        
        // Get variables from all functions (simplified - doesn't handle scope properly)
        for (func_name, function) in &hir.functions {
            // Add function parameters
            for param in &function.parameters {
                let type_str = format!("{:?}", param.ty); // Simple type representation
                variables.push((param.name.clone(), type_str));
            }
            
            // Add local variables from var_names if available
            for (i, var_type) in function.body.vars.iter().enumerate() {
                if let Some(var_name) = function.body.var_names.get(i).and_then(|name| name.as_ref()) {
                    let type_str = format!("{:?}", var_type);
                    variables.push((var_name.clone(), type_str));
                }
            }
        }
        
        Some(variables)
    }


}