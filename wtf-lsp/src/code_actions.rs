use std::collections::HashMap;
use tower_lsp::lsp_types::*;
use wtf_ast;

use crate::Backend;

impl Backend {
    pub async fn provide_code_actions(&self, params: CodeActionParams) -> Option<Vec<CodeActionOrCommand>> {
        let uri = &params.text_document.uri;
        let documents = self.documents.read().await;
        let text = documents.get(uri)?;
        
        let mut all_actions = Vec::new();
        
        // Process each diagnostic to generate corresponding code actions
        for diagnostic in &params.context.diagnostics {
            if let Some(mut actions) = self.generate_code_actions_for_diagnostic(diagnostic, text, uri).await {
                all_actions.append(&mut actions);
            }
        }
        
        if all_actions.is_empty() {
            None
        } else {
            Some(all_actions)
        }
    }

    async fn generate_code_actions_for_diagnostic(&self, diagnostic: &Diagnostic, text: &str, uri: &Url) -> Option<Vec<CodeActionOrCommand>> {
        // Check for unknown field errors and generate suggestions
        if diagnostic.message.contains("Field") && diagnostic.message.contains("does not exist") {
            return self.generate_unknown_field_actions(diagnostic, text, uri).await;
        }
        
        // Check for syntax errors that might be cross-language keyword issues
        if diagnostic.message.contains("Unexpected") || diagnostic.message.contains("expected") {
            return self.generate_keyword_actions(diagnostic, text, uri);
        }
        
        // Check for field access errors based on dot notation patterns
        if let Some(actions) = self.generate_field_access_actions(diagnostic, text, uri).await {
            return Some(actions);
        }
        
        None
    }

    async fn generate_field_access_actions(&self, diagnostic: &Diagnostic, text: &str, uri: &Url) -> Option<Vec<CodeActionOrCommand>> {
        // Check for potential field access errors based on code structure
        let chars: Vec<char> = text.chars().collect();
        let line_start = self.find_line_start(diagnostic.range.start.line, &chars);
        let line_end = self.find_line_end(diagnostic.range.start.line, &chars);
        
        if line_end <= line_start {
            return None;
        }
        
        let line_text: String = chars[line_start..line_end].iter().collect();
        
        // Look for dot notation patterns: "identifier.something"
        if let Some(dot_pos) = line_text.rfind('.') {
            let after_dot = &line_text[dot_pos + 1..].trim();
            
            // Check if this looks like an identifier after a dot
            if after_dot.chars().all(|c| c.is_alphanumeric() || c == '_') && !after_dot.is_empty() {
                // Find the identifier before the dot
                let before_dot = &line_text[..dot_pos];
                if let Some(space_pos) = before_dot.rfind(char::is_whitespace) {
                    let identifier = before_dot[space_pos..].trim();
                    
                    // Try to infer the type of the identifier using HIR
                    if let Some(type_name) = self.infer_expression_type_hir(identifier, uri).await {
                        // Get valid field names for this type
                        let ast_cache = self.ast_cache.read().await;
                        if let Some(ast) = ast_cache.get(uri) {
                            let field_names = self.get_type_fields(ast, &type_name);
                            drop(ast_cache);
                            
                            if !field_names.is_empty() {
                                // Generate field suggestions
                                let mut actions = Vec::new();
                                for field in field_names {
                                    let action = CodeActionOrCommand::CodeAction(CodeAction {
                                        title: format!("Change to '{}'", field),
                                        kind: Some(CodeActionKind::QUICKFIX),
                                        diagnostics: Some(vec![diagnostic.clone()]),
                                        edit: Some(WorkspaceEdit {
                                            changes: Some({
                                                let mut changes = HashMap::new();
                                                // Calculate the range for the field name after the dot
                                                let field_start_pos = Position::new(
                                                    diagnostic.range.start.line,
                                                    diagnostic.range.start.character.saturating_sub(line_start as u32) + (dot_pos + 1) as u32
                                                );
                                                let field_end_pos = Position::new(
                                                    diagnostic.range.start.line,
                                                    field_start_pos.character + after_dot.len() as u32
                                                );
                                                let field_range = Range::new(field_start_pos, field_end_pos);
                                                
                                                changes.insert(uri.clone(), vec![TextEdit {
                                                    range: field_range,
                                                    new_text: field,
                                                }]);
                                                changes
                                            }),
                                            ..Default::default()
                                        }),
                                        ..Default::default()
                                    });
                                    actions.push(action);
                                }
                                return Some(actions);
                            }
                        }
                    }
                }
            }
        }
        
        None
    }

    async fn generate_unknown_field_actions(&self, diagnostic: &Diagnostic, _text: &str, uri: &Url) -> Option<Vec<CodeActionOrCommand>> {
        // Extract field name and type name from the error message
        let message = &diagnostic.message;
        
        // Parse the error message to extract type information
        // Format: "Field 'field_name' does not exist on type 'type_name' at (start, end)"
        let type_name = if let Some(start) = message.find("on type '") {
            let after_type = &message[start + 9..];
            if let Some(end) = after_type.find("'") {
                Some(&after_type[..end])
            } else {
                None
            }
        } else {
            None
        };

        let Some(type_name) = type_name else {
            return None;
        };

        // Get the cached AST to find actual field names
        let ast_cache = self.ast_cache.read().await;
        let Some(ast) = ast_cache.get(uri) else {
            return None;
        };

        // Find the type declaration and extract its fields
        let field_names = self.get_type_fields(ast, type_name);
        
        if field_names.is_empty() {
            return None;
        }

        let mut actions = Vec::new();
        for field in field_names {
            let action = CodeActionOrCommand::CodeAction(CodeAction {
                title: format!("Change to '{}'", field),
                kind: Some(CodeActionKind::QUICKFIX),
                diagnostics: Some(vec![diagnostic.clone()]),
                edit: Some(WorkspaceEdit {
                    changes: Some({
                        let mut changes = HashMap::new();
                        changes.insert(uri.clone(), vec![TextEdit {
                            range: diagnostic.range,
                            new_text: field,
                        }]);
                        changes
                    }),
                    ..Default::default()
                }),
                ..Default::default()
            });
            actions.push(action);
        }

        Some(actions)
    }

    fn generate_keyword_actions(&self, diagnostic: &Diagnostic, text: &str, uri: &Url) -> Option<Vec<CodeActionOrCommand>> {
        let mut actions = Vec::new();

        // Extract the problematic token from the range
        let source_chars: Vec<char> = text.chars().collect();
        let start_offset = self.position_to_byte(diagnostic.range.start, &source_chars);
        let end_offset = self.position_to_byte(diagnostic.range.end, &source_chars);
        
        if start_offset < source_chars.len() && end_offset <= source_chars.len() {
            let found_token: String = source_chars[start_offset..end_offset].iter().collect();
            
            // Map common keywords from other languages to WTF equivalents
            let keyword_mappings = self.get_keyword_mappings();
            
            // Only show similar tokens from other languages, not expected tokens
            if let Some(wtf_keywords) = keyword_mappings.get(&found_token.as_str()) {
                for wtf_keyword in wtf_keywords {
                    let action = CodeActionOrCommand::CodeAction(CodeAction {
                        title: format!("Change '{}' to '{}'", found_token, wtf_keyword),
                        kind: Some(CodeActionKind::QUICKFIX),
                        diagnostics: Some(vec![diagnostic.clone()]),
                        edit: Some(WorkspaceEdit {
                            changes: Some({
                                let mut changes = HashMap::new();
                                changes.insert(uri.clone(), vec![TextEdit {
                                    range: diagnostic.range,
                                    new_text: wtf_keyword.to_string(),
                                }]);
                                changes
                            }),
                            ..Default::default()
                        }),
                        ..Default::default()
                    });
                    actions.push(action);
                }
            }
        }

        if actions.is_empty() {
            None
        } else {
            Some(actions)
        }
    }

    pub fn get_keyword_mappings(&self) -> HashMap<&'static str, Vec<&'static str>> {
        let mut mappings = HashMap::new();
        
        // Function keywords
        mappings.insert("def", vec!["func"]);
        mappings.insert("fn", vec!["func"]);
        mappings.insert("fun", vec!["func"]);
        mappings.insert("function", vec!["func"]);
        
        // Type keywords
        mappings.insert("struct", vec!["record"]);
        mappings.insert("class", vec!["resource", "record"]);
        mappings.insert("interface", vec!["resource"]);
        mappings.insert("union", vec!["variant"]);
        mappings.insert("sum", vec!["variant"]);
        
        // Variable keywords
        mappings.insert("const", vec!["let"]);
        mappings.insert("mut", vec!["var"]);
        mappings.insert("mutable", vec!["var"]);
        
        // Control flow
        mappings.insert("elif", vec!["else if"]);
        mappings.insert("elseif", vec!["else if"]);
        mappings.insert("switch", vec!["match"]);
        mappings.insert("case", vec!["match"]);
        mappings.insert("default", vec!["else"]);
        
        mappings
    }


}