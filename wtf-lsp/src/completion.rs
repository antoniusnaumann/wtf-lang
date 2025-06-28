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
        
        // TODO: Add other completion types (keywords, identifiers, etc.)
        
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
            return completions;
        }
        
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


}