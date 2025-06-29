//! WTF Language Server
//! 
//! A Language Server Protocol (LSP) implementation for the WTF programming language.
//! 
//! This crate provides intelligent language support including:
//! - Real-time syntax and semantic error detection
//! - Context-aware auto-completion with UFCS and structural typing
//! - Quick fixes and refactoring actions
//! - HIR-based type inference with AST fallbacks
//! 
//! The language server is organized into specialized modules:
//! - `completion`: Auto-completion and IntelliSense features
//! - `diagnostics`: Error detection and reporting
//! - `code_actions`: Quick fixes and refactoring capabilities
//! - `type_inference`: Accurate type resolution using HIR and AST
//! 
//! See ARCHITECTURE.md and docs/ for comprehensive documentation.

use std::collections::HashMap;
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer};
use wtf_hir;
use wtf_ast;

mod completion;
mod diagnostics;
mod code_actions;
mod type_inference;

#[derive(Debug)]
pub struct Backend {
    client: Client,
    documents: tokio::sync::RwLock<HashMap<Url, String>>,
    // Store AST for type information access
    pub ast_cache: tokio::sync::RwLock<HashMap<Url, wtf_ast::Module>>,
    // Store HIR for accurate type inference
    pub hir_cache: tokio::sync::RwLock<HashMap<Url, wtf_hir::Module>>,
}

impl Backend {
    pub fn new(client: Client) -> Self {
        Self {
            client,
            documents: tokio::sync::RwLock::new(HashMap::new()),
            ast_cache: tokio::sync::RwLock::new(HashMap::new()),
            hir_cache: tokio::sync::RwLock::new(HashMap::new()),
        }
    }

    // Methods that are shared across modules and needed for AST operations
    pub fn get_type_fields(&self, ast: &wtf_ast::Module, type_name: &str) -> Vec<String> {
        // Check if this is an anonymous record type
        if type_name.starts_with("AnonymousRecord:") {
            if let Some(fields_part) = type_name.strip_prefix("AnonymousRecord:") {
                if !fields_part.is_empty() {
                    return fields_part.split(',').map(|s| s.to_string()).collect();
                }
            }
            return Vec::new();
        }
        
        // Handle declared record/resource types
        for declaration in &ast.declarations {
            match declaration {
                wtf_ast::Declaration::Record(record) if record.name == type_name => {
                    return record.fields.iter().map(|f| f.name.clone()).collect();
                },
                wtf_ast::Declaration::Resource(resource) if resource.name == type_name => {
                    return resource.fields.iter().map(|f| f.name.clone()).collect();
                },
                _ => {}
            }
        }
        Vec::new()
    }

    pub fn get_type_methods(&self, ast: &wtf_ast::Module, type_name: &str) -> Vec<(String, String)> {
        let mut methods = Vec::new();
        
        // Find resource methods
        for declaration in &ast.declarations {
            if let wtf_ast::Declaration::Resource(resource) = declaration {
                if resource.name == type_name {
                    for method in &resource.methods {
                        let signature = self.format_function_signature(method);
                        methods.push((method.name.clone(), signature));
                    }
                }
            }
        }
        
        methods
    }

    pub fn get_ufcs_methods(&self, ast: &wtf_ast::Module, type_name: &str) -> Vec<(String, String)> {
        let mut methods = Vec::new();
        
        // Find functions where the first parameter matches the type (including structural typing)
        for declaration in &ast.declarations {
            if let wtf_ast::Declaration::Function(func) = declaration {
                if let Some(first_param) = func.parameters.first() {
                    if self.type_is_structurally_compatible(ast, type_name, Some(&first_param.type_annotation)) {
                        let signature = self.format_function_signature(func);
                        methods.push((func.name.clone(), signature));
                    }
                }
            }
        }
        
        methods
    }

    /// Check if `source_type` is structurally compatible with `target_type_annotation`.
    /// For structural typing, this means the source type has all fields required by the target type.
    fn type_is_structurally_compatible(&self, ast: &wtf_ast::Module, source_type: &str, target_type_annotation: Option<&wtf_ast::TypeAnnotation>) -> bool {
        // First check for exact matches
        if self.type_annotation_matches(target_type_annotation, source_type) {
            return true;
        }
        
        // Get the fields of the source type
        let source_fields = self.get_type_fields(ast, source_type);
        if source_fields.is_empty() {
            return false;
        }
        
        // Get the fields required by the target type
        let target_fields = match target_type_annotation {
            Some(annotation) => match &annotation.kind {
                wtf_ast::TypeAnnotationKind::Simple(target_name) => {
                    self.get_type_fields(ast, target_name)
                },
                _ => Vec::new(),
            },
            None => Vec::new(),
        };
        
        if target_fields.is_empty() {
            return false;
        }
        
        // Check if source type has all required fields (structural typing)
        target_fields.iter().all(|target_field| source_fields.contains(target_field))
    }



    fn format_function_signature(&self, func: &wtf_ast::FunctionDeclaration) -> String {
        let params: Vec<String> = func.parameters.iter()
            .map(|p| format!("{}: {}", p.name, self.format_type_annotation(&p.type_annotation)))
            .collect();
        
        let return_type = func.return_type.as_ref()
            .map(|t| format!(" -> {}", self.format_type_annotation(t)))
            .unwrap_or_default();
        
        format!("({}){}", params.join(", "), return_type)
    }

    fn format_type_annotation(&self, type_annotation: &wtf_ast::TypeAnnotation) -> String {
        match &type_annotation.kind {
            wtf_ast::TypeAnnotationKind::Simple(name) => name.clone(),
            wtf_ast::TypeAnnotationKind::List(inner) => {
                format!("[{}]", self.format_type_annotation(inner))
            },
            wtf_ast::TypeAnnotationKind::Option(inner) => {
                format!("{}?", self.format_type_annotation(inner))
            },
            _ => "unknown".to_string(),
        }
    }

    fn type_annotation_matches(&self, type_annotation: Option<&wtf_ast::TypeAnnotation>, type_name: &str) -> bool {
        match type_annotation {
            Some(annotation) => match &annotation.kind {
                wtf_ast::TypeAnnotationKind::Simple(name) => name == type_name,
                _ => false,
            },
            None => false,
        }
    }

    // Shared helper methods for line navigation
    pub fn find_line_start(&self, line: u32, chars: &[char]) -> usize {
        let mut current_line = 0;
        for (i, &ch) in chars.iter().enumerate() {
            if current_line == line {
                return i;
            }
            if ch == '\n' {
                current_line += 1;
            }
        }
        chars.len()
    }

    pub fn find_line_end(&self, line: u32, chars: &[char]) -> usize {
        let mut current_line = 0;
        for (i, &ch) in chars.iter().enumerate() {
            if current_line == line && ch == '\n' {
                return i;
            }
            if ch == '\n' {
                current_line += 1;
            }
        }
        chars.len()
    }

    pub fn position_to_byte(&self, position: Position, chars: &[char]) -> usize {
        let line_start = self.find_line_start(position.line, chars);
        let mut byte_pos = line_start;
        let mut char_count = 0;
        
        for &ch in &chars[line_start..] {
            if char_count >= position.character as usize {
                break;
            }
            if ch == '\n' {
                break;
            }
            byte_pos += 1;
            char_count += 1;
        }
        
        byte_pos.min(chars.len())
    }
}

#[tower_lsp::async_trait]
impl LanguageServer for Backend {
    async fn initialize(&self, _: InitializeParams) -> Result<InitializeResult> {
        Ok(InitializeResult {
            capabilities: ServerCapabilities {
                text_document_sync: Some(TextDocumentSyncCapability::Kind(
                    TextDocumentSyncKind::FULL,
                )),
                completion_provider: Some(CompletionOptions {
                    resolve_provider: Some(false),
                    trigger_characters: Some(vec![".".to_string()]),
                    work_done_progress_options: Default::default(),
                    all_commit_characters: None,
                    completion_item: None,
                }),
                code_action_provider: Some(CodeActionProviderCapability::Simple(true)),
                ..ServerCapabilities::default()
            },
            ..Default::default()
        })
    }

    async fn initialized(&self, _: InitializedParams) {
        self.client
            .log_message(MessageType::INFO, "WTF language server initialized!")
            .await;
    }

    async fn shutdown(&self) -> Result<()> {
        Ok(())
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        let uri = params.text_document.uri;
        let text = params.text_document.text;
        
        self.documents.write().await.insert(uri.clone(), text.clone());
        
        let diagnostics = self.validate_document(&uri, &text).await;
        self.client.publish_diagnostics(uri, diagnostics, None).await;
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        let uri = params.text_document.uri;
        if let Some(change) = params.content_changes.into_iter().next() {
            let text = change.text;
            self.documents.write().await.insert(uri.clone(), text.clone());
            
            let diagnostics = self.validate_document(&uri, &text).await;
            self.client.publish_diagnostics(uri, diagnostics, None).await;
        }
    }

    async fn completion(&self, params: CompletionParams) -> Result<Option<CompletionResponse>> {
        Ok(self.provide_completion(params).await)
    }

    async fn code_action(&self, params: CodeActionParams) -> Result<Option<CodeActionResponse>> {
        Ok(self.provide_code_actions(params).await.map(CodeActionResponse::from))
    }
}

#[cfg(test)]
mod tests {
    use wtf_ast::{TypeAnnotation, TypeAnnotationKind};
    use crate::Backend;

    #[tokio::test]
    async fn test_keyword_mappings_logic() {
        // Create a mock client for testing
        let (_service, _socket) = tower_lsp::LspService::new(|client| Backend::new(client));
        let backend = _service.inner();
        let mappings = backend.get_keyword_mappings();
        
        // Test some basic mappings
        assert!(mappings.contains_key("def"));
        assert!(mappings.get("def").unwrap().contains(&"func"));
        
        assert!(mappings.contains_key("struct"));
        assert!(mappings.get("struct").unwrap().contains(&"record"));
    }

    #[test]
    fn test_type_annotation_formatting() {
        // Create a mock client for testing
        let (_service, _socket) = tower_lsp::LspService::new(|client| Backend::new(client));
        let backend = _service.inner();
        
        let simple_annotation = TypeAnnotation {
            kind: TypeAnnotationKind::Simple("String".to_string()),
            span: wtf_tokens::Span { start: 0, end: 6 }
        };
        
        let list_annotation = TypeAnnotation {
            kind: TypeAnnotationKind::List(Box::new(simple_annotation.clone())),
            span: wtf_tokens::Span { start: 0, end: 8 }
        };
        
        assert_eq!(backend.format_type_annotation(&simple_annotation), "String");
        assert_eq!(backend.format_type_annotation(&list_annotation), "[String]");
    }
}