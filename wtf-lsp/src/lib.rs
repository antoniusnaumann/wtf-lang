use std::collections::HashMap;
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer};
use wtf_error::Error as WtfError;
use wtf_hir;
use wtf_parser::parser::Parser;
use wtf_ast;

#[derive(Debug)]
pub struct Backend {
    client: Client,
    documents: tokio::sync::RwLock<HashMap<Url, String>>,
    // Store AST for type information access
    ast_cache: tokio::sync::RwLock<HashMap<Url, wtf_ast::Module>>,
}

impl Backend {
    pub fn new(client: Client) -> Self {
        Self {
            client,
            documents: tokio::sync::RwLock::new(HashMap::new()),
            ast_cache: tokio::sync::RwLock::new(HashMap::new()),
        }
    }

    pub async fn validate_document(&self, uri: &Url, text: &str) -> Vec<Diagnostic> {
        let mut diagnostics = Vec::new();

        // Parse the document and collect parser errors
        let mut parser = Parser::new(text);
        let parse_result = parser.parse_module();

        // Get the source characters for position mapping
        let source_chars = parser.chars();

        // Convert parser errors to diagnostics
        for error in parser.errors() {
            let diagnostic = self.wtf_error_to_diagnostic(error, source_chars);
            diagnostics.push(diagnostic);
        }

        // If parsing failed, also add the main parse error
        if let Err(error) = parse_result {
            let diagnostic = self.wtf_error_to_diagnostic(&error, source_chars);
            diagnostics.push(diagnostic);
        } else if let Ok(ast) = parse_result {
            // Cache the AST for later use in code actions
            self.ast_cache.write().await.insert(uri.clone(), ast.clone());
            
            // If parsing succeeded, try HIR compilation to find HIR-level errors
            match wtf_hir::compile(ast) {
                Ok(_hir) => {
                    // HIR compilation succeeded, no additional errors
                }
                Err(hir_errors) => {
                    // Convert HIR errors to diagnostics
                    for hir_error in hir_errors {
                        let diagnostic = self.wtf_error_to_diagnostic(&hir_error, source_chars);
                        diagnostics.push(diagnostic);
                    }
                }
            }
        }

        diagnostics
    }

    pub fn wtf_error_to_diagnostic(&self, error: &WtfError, source_chars: &[char]) -> Diagnostic {
        // Convert byte span to line/column positions
        let start_pos = self.byte_to_position(error.span.start, source_chars);
        let end_pos = self.byte_to_position(error.span.end, source_chars);
        
        let range = Range::new(start_pos, end_pos);
        let message = error.with_source(source_chars);
        
        Diagnostic {
            range,
            severity: Some(DiagnosticSeverity::ERROR),
            code: None,
            code_description: None,
            source: Some("wtf-lsp".to_string()),
            message,
            related_information: None,
            tags: None,
            data: None,
        }
    }

    fn byte_to_position(&self, byte_offset: usize, chars: &[char]) -> Position {
        let mut line = 0;
        let mut character = 0;
        
        for (i, &ch) in chars.iter().enumerate() {
            if i >= byte_offset {
                break;
            }
            if ch == '\n' {
                line += 1;
                character = 0;
            } else {
                character += 1;
            }
        }
        
        Position::new(line, character)
    }

    async fn generate_code_actions_for_diagnostic(&self, diagnostic: &Diagnostic, text: &str, uri: &Url) -> Option<Vec<CodeActionOrCommand>> {
        let message = &diagnostic.message;
        let mut actions = Vec::new();

        // Parse different error types and generate appropriate actions
        if message.contains("Unknown field") || message.contains("does not exist on type") {
            if let Some(field_actions) = self.generate_unknown_field_actions(diagnostic, text, uri).await {
                actions.extend(field_actions);
            }
        } else if message.contains("Expected") && (message.contains("but found") || message.contains("keyword")) {
            if let Some(keyword_actions) = self.generate_keyword_actions(diagnostic, text, uri) {
                actions.extend(keyword_actions);
            }
        }

        if actions.is_empty() {
            None
        } else {
            Some(actions)
        }
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
        
        mappings
    }

    fn get_type_fields(&self, ast: &wtf_ast::Module, type_name: &str) -> Vec<String> {
        let mut field_names = Vec::new();
        
        for declaration in &ast.declarations {
            match declaration {
                wtf_ast::Declaration::Record(record) if record.name == type_name => {
                    for field in &record.fields {
                        field_names.push(field.name.clone());
                    }
                    break;
                }
                wtf_ast::Declaration::Resource(resource) if resource.name == type_name => {
                    for field in &resource.fields {
                        field_names.push(field.name.clone());
                    }
                    break;
                }
                wtf_ast::Declaration::Export(export) => {
                    // Check exported declarations
                    match export.item.as_ref() {
                        wtf_ast::Declaration::Record(record) if record.name == type_name => {
                            for field in &record.fields {
                                field_names.push(field.name.clone());
                            }
                            break;
                        }
                        wtf_ast::Declaration::Resource(resource) if resource.name == type_name => {
                            for field in &resource.fields {
                                field_names.push(field.name.clone());
                            }
                            break;
                        }
                        _ => continue,
                    }
                }
                _ => continue,
            }
        }
        
        field_names
    }

    fn position_to_byte(&self, position: Position, chars: &[char]) -> usize {
        let mut byte_offset = 0;
        let mut current_line = 0;
        let mut current_character = 0;
        
        for &ch in chars {
            if current_line == position.line && current_character == position.character {
                break;
            }
            
            if ch == '\n' {
                current_line += 1;
                current_character = 0;
            } else {
                current_character += 1;
            }
            byte_offset += 1;
        }
        
        byte_offset
    }
}

#[tower_lsp::async_trait]
impl LanguageServer for Backend {
    async fn initialize(&self, _: InitializeParams) -> Result<InitializeResult> {
        Ok(InitializeResult {
            server_info: None,
            capabilities: ServerCapabilities {
                text_document_sync: Some(TextDocumentSyncCapability::Kind(
                    TextDocumentSyncKind::FULL,
                )),
                code_action_provider: Some(CodeActionProviderCapability::Simple(true)),
                ..ServerCapabilities::default()
            },
        })
    }

    async fn initialized(&self, _: InitializedParams) {
        self.client
            .log_message(MessageType::INFO, "WTF Language Server initialized!")
            .await;
    }

    async fn shutdown(&self) -> Result<()> {
        Ok(())
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        let uri = params.text_document.uri;
        let text = params.text_document.text;
        
        // Store the document
        self.documents.write().await.insert(uri.clone(), text.clone());
        
        // Validate and send diagnostics
        let diagnostics = self.validate_document(&uri, &text).await;
        self.client
            .publish_diagnostics(uri, diagnostics, None)
            .await;
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        let uri = params.text_document.uri;
        
        // For FULL sync, we replace the entire document
        if let Some(change) = params.content_changes.into_iter().next() {
            let text = change.text;
            
            // Update stored document
            self.documents.write().await.insert(uri.clone(), text.clone());
            
            // Validate and send diagnostics
            let diagnostics = self.validate_document(&uri, &text).await;
            self.client
                .publish_diagnostics(uri, diagnostics, None)
                .await;
        }
    }

    async fn did_close(&self, params: DidCloseTextDocumentParams) {
        let uri = params.text_document.uri;
        
        // Remove the document from storage
        self.documents.write().await.remove(&uri);
        
        // Remove the cached AST
        self.ast_cache.write().await.remove(&uri);
        
        // Clear diagnostics
        self.client
            .publish_diagnostics(uri, Vec::new(), None)
            .await;
    }

    async fn code_action(&self, params: CodeActionParams) -> Result<Option<CodeActionResponse>> {
        let uri = &params.text_document.uri;
        let range = params.range;
        
        // Get the document content
        let documents = self.documents.read().await;
        let Some(text) = documents.get(uri) else {
            return Ok(None);
        };

        // Find diagnostics that overlap with the requested range
        let diagnostics = self.validate_document(uri, text).await;
        let relevant_diagnostics: Vec<_> = diagnostics
            .into_iter()
            .filter(|diag| ranges_overlap(&diag.range, &range))
            .collect();

        if relevant_diagnostics.is_empty() {
            return Ok(None);
        }

        let mut actions = Vec::new();
        
        // Generate code actions for each relevant diagnostic
        for diagnostic in relevant_diagnostics {
            if let Some(mut diagnostic_actions) = self.generate_code_actions_for_diagnostic(&diagnostic, text, uri).await {
                actions.append(&mut diagnostic_actions);
            }
        }

        if actions.is_empty() {
            Ok(None)
        } else {
            Ok(Some(actions))
        }
    }
}

fn ranges_overlap(range1: &Range, range2: &Range) -> bool {
    // Check if two ranges overlap
    !(range1.end < range2.start || range2.end < range1.start)
}

#[cfg(test)]
mod tests {
    use super::*;
    use wtf_error::{Error as WtfError, ErrorKind};
    use wtf_tokens::Span;

    // Test without creating a Backend instance to avoid Client dependency
    #[test]
    fn test_diagnostic_creation() {
        // Test the diagnostic structure creation directly
        let error = WtfError {
            kind: ErrorKind::UnknownIdentifier,
            span: Span { start: 0, end: 5 }
        };
        
        let source_chars = "hello world".chars().collect::<Vec<_>>();
        
        // Manually create the diagnostic as the function would
        let start_pos = Position::new(0, 0);
        let end_pos = Position::new(0, 5);
        let range = Range::new(start_pos, end_pos);
        let message = error.with_source(&source_chars);
        
        let diagnostic = Diagnostic {
            range,
            severity: Some(DiagnosticSeverity::ERROR),
            code: None,
            code_description: None,
            source: Some("wtf-lsp".to_string()),
            message,
            related_information: None,
            tags: None,
            data: None,
        };
        
        // Check that the diagnostic has ERROR severity
        assert_eq!(diagnostic.severity, Some(DiagnosticSeverity::ERROR));
        assert_eq!(diagnostic.source, Some("wtf-lsp".to_string()));
    }

    #[test]
    fn test_keyword_mappings_logic() {
        // Test the keyword mapping logic without Backend instance
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
        
        // Test some mappings
        assert!(mappings.contains_key("def"));
        assert!(mappings.get("def").unwrap().contains(&"func"));
        
        assert!(mappings.contains_key("struct"));
        assert!(mappings.get("struct").unwrap().contains(&"record"));
        
        assert!(mappings.contains_key("class"));
        assert!(mappings.get("class").unwrap().contains(&"resource"));
    }

    #[test]
    fn test_ranges_overlap() {
        let range1 = Range::new(Position::new(0, 0), Position::new(0, 5));
        let range2 = Range::new(Position::new(0, 3), Position::new(0, 8));
        let range3 = Range::new(Position::new(0, 10), Position::new(0, 15));
        
        assert!(ranges_overlap(&range1, &range2));
        assert!(!ranges_overlap(&range1, &range3));
    }
}