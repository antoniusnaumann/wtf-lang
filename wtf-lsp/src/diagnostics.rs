//! Diagnostics Module
//! 
//! Handles error detection and reporting for the WTF Language Server.
//! 
//! This module integrates with the WTF compiler pipeline to provide real-time
//! error reporting including:
//! - Syntax error detection via the WTF parser
//! - Semantic error detection via HIR compilation
//! - Precise position mapping from byte offsets to LSP positions
//! - Proper diagnostic severity classification and source attribution
//! - Graceful error handling and recovery strategies
//! 
//! The diagnostics system ensures consistent error reporting with ERROR severity
//! for all WTF language issues, enabling proper IDE integration and user experience.
//! 
//! See docs/diagnostics.md for detailed implementation documentation.

use tower_lsp::lsp_types::*;
use wtf_error::Error as WtfError;
use wtf_hir;
use wtf_parser::parser::Parser;

use crate::Backend;

impl Backend {
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

        // Try to cache the AST even if there are some errors
        match parse_result {
            Ok(ast) => {
                // Cache the successful AST
                self.ast_cache.write().await.insert(uri.clone(), ast.clone());
                
                // If parsing succeeded, try HIR compilation to find HIR-level errors
                // But catch panics from unimplemented features
                let hir_result = std::panic::catch_unwind(|| {
                    wtf_hir::compile(ast)
                });
                
                match hir_result {
                    Ok(Ok(hir)) => {
                        // HIR compilation succeeded, cache it for type inference
                        self.hir_cache.write().await.insert(uri.clone(), hir);
                    }
                    Ok(Err(hir_errors)) => {
                        // Convert HIR errors to diagnostics, but filter out "not yet implemented" errors
                        for hir_error in hir_errors {
                            let error_message = hir_error.with_source(source_chars);
                            
                            // Skip "not yet implemented" errors as they're not user errors
                            if !error_message.contains("not yet implemented") {
                                let diagnostic = self.wtf_error_to_diagnostic(&hir_error, source_chars);
                                diagnostics.push(diagnostic);
                            }
                        }
                    }
                    Err(_panic) => {
                        // HIR compilation panicked (likely due to unimplemented features)
                        // This is not a user error, so we don't add any diagnostics
                    }
                }
            }
            Err(error) => {
                // Add the main parse error
                let diagnostic = self.wtf_error_to_diagnostic(&error, source_chars);
                diagnostics.push(diagnostic);
                
                // Try to parse with recovery to get partial AST for completion
                // For now, we'll just try parsing up to the error point
                if let Some(partial_ast) = self.try_partial_parse(text, uri).await {
                    self.ast_cache.write().await.insert(uri.clone(), partial_ast);
                }
            }
        }

        diagnostics
    }

    async fn try_partial_parse(&self, text: &str, _uri: &Url) -> Option<wtf_ast::Module> {
        // Try to parse by removing incomplete lines or expressions
        let lines: Vec<&str> = text.lines().collect();
        
        // Try removing lines one by one from the end until we get a successful parse
        for i in (1..=lines.len()).rev() {
            let partial_text = lines[..i].join("\n");
            
            // Skip if the last line ends with incomplete syntax
            if partial_text.trim_end().ends_with('.') || 
               partial_text.trim_end().ends_with("let ") ||
               partial_text.trim_end().ends_with("= ") {
                continue;
            }
            
            let mut parser = Parser::new(&partial_text);
            if let Ok(ast) = parser.parse_module() {
                // Only accept if there are minimal errors
                if parser.errors().len() <= 1 {
                    return Some(ast);
                }
            }
        }
        
        None
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

    pub fn byte_to_position(&self, byte_offset: usize, chars: &[char]) -> Position {
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
        
        Position::new(line as u32, character as u32)
    }
}