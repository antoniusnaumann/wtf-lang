use std::collections::HashMap;
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer};
use wtf_error::Error as WtfError;
use wtf_hir;
use wtf_parser::parser::Parser;

#[derive(Debug)]
pub struct Backend {
    client: Client,
    documents: tokio::sync::RwLock<HashMap<Url, String>>,
}

impl Backend {
    pub fn new(client: Client) -> Self {
        Self {
            client,
            documents: tokio::sync::RwLock::new(HashMap::new()),
        }
    }

    pub async fn validate_document(&self, _uri: &Url, text: &str) -> Vec<Diagnostic> {
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
            // If parsing succeeded, try HIR compilation to find HIR-level errors
            // Note: The current HIR compiler doesn't return errors, it panics on them
            // For now, we'll catch any panics and convert them to diagnostics
            // In a real implementation, we'd need to modify the HIR compiler to return errors
            match std::panic::catch_unwind(|| wtf_hir::compile(ast)) {
                Ok(_hir) => {
                    // HIR compilation succeeded, no additional errors
                }
                Err(_) => {
                    // HIR compilation panicked, add a generic error
                    let diagnostic = Diagnostic::new_simple(
                        Range::new(Position::new(0, 0), Position::new(0, 0)),
                        "HIR compilation failed".to_string(),
                    );
                    diagnostics.push(diagnostic);
                }
            }
        }

        diagnostics
    }

    fn wtf_error_to_diagnostic(&self, error: &WtfError, source_chars: &[char]) -> Diagnostic {
        // Convert byte span to line/column positions
        let start_pos = self.byte_to_position(error.span.start, source_chars);
        let end_pos = self.byte_to_position(error.span.end, source_chars);
        
        let range = Range::new(start_pos, end_pos);
        let message = error.with_source(source_chars);
        
        Diagnostic::new_simple(range, message)
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
        
        // Clear diagnostics
        self.client
            .publish_diagnostics(uri, Vec::new(), None)
            .await;
    }
}