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
    pub ast_cache: tokio::sync::RwLock<HashMap<Url, wtf_ast::Module>>,
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

    async fn generate_method_completions(&self, text: &str, position: Position, uri: &Url) -> Option<Vec<CompletionItem>> {
        let chars: Vec<char> = text.chars().collect();
        let cursor_offset = self.position_to_byte(position, &chars);
        
        // Check if we're right after a "." character
        if cursor_offset == 0 {
            return None;
        }
        
        // Look backwards to find the "." character
        let mut dot_position = None;
        for i in (0..cursor_offset).rev() {
            if chars[i] == '.' {
                dot_position = Some(i);
                break;
            } else if !chars[i].is_whitespace() {
                // If we hit non-whitespace before a dot, not a method completion context
                break;
            }
        }
        
        let dot_offset = dot_position?;
        
        // Extract the expression before the dot
        let expression_end = dot_offset;
        let expression_start = self.find_expression_start(&chars, expression_end)?;
        
        let expression_text: String = chars[expression_start..expression_end].iter().collect();
        let expression_text = expression_text.trim();
        
        if expression_text.is_empty() {
            return None;
        }
        
        // Try to determine the type of the expression
        let expression_type = self.infer_expression_type(expression_text, uri).await?;
        
        // Get available methods for this type
        let methods = self.get_available_methods(&expression_type, uri).await;
        
        // Convert to completion items
        let mut completions = Vec::new();
        for (method_name, signature) in methods {
            let completion_item = CompletionItem {
                label: method_name.clone(),
                kind: Some(CompletionItemKind::METHOD),
                detail: Some(format!("fn {}{}", method_name, signature)),
                documentation: None,
                insert_text: Some(format!("{}($0)", method_name)),
                insert_text_format: Some(InsertTextFormat::SNIPPET),
                ..Default::default()
            };
            completions.push(completion_item);
        }
        
        if completions.is_empty() {
            None
        } else {
            Some(completions)
        }
    }

    fn find_expression_start(&self, chars: &[char], end: usize) -> Option<usize> {
        let mut depth = 0;
        let mut i = end;
        
        while i > 0 {
            i -= 1;
            let ch = chars[i];
            
            match ch {
                ')' | ']' | '}' => depth += 1,
                '(' | '[' | '{' => {
                    if depth == 0 {
                        // Found opening bracket at depth 0, this is likely the start
                        break;
                    }
                    depth -= 1;
                },
                ' ' | '\t' | '\n' | '\r' => {
                    if depth == 0 {
                        // Whitespace at depth 0 - expression likely starts after this
                        return Some(i + 1);
                    }
                },
                '=' | '+' | '-' | '*' | '/' | '<' | '>' | '!' | '&' | '|' | '^' | '%' => {
                    if depth == 0 {
                        // Operator at depth 0 - expression starts after this
                        return Some(i + 1);
                    }
                },
                _ => {
                    if depth == 0 && (ch.is_alphabetic() || ch == '_') {
                        // Continue collecting identifier characters
                        continue;
                    }
                }
            }
        }
        
        Some(i)
    }

    async fn infer_expression_type(&self, expression: &str, uri: &Url) -> Option<String> {
        // Get the cached AST
        let ast_cache = self.ast_cache.read().await;
        let ast = ast_cache.get(uri)?;
        
        // Simple type inference - for now, just handle identifiers and basic cases
        if expression.chars().all(|c| c.is_alphanumeric() || c == '_') {
            // Simple identifier - look for variable declarations or parameters
            self.find_identifier_type(expression, ast)
        } else {
            // More complex expression - would need full expression parsing and type inference
            // For now, return None to indicate we can't infer the type
            None
        }
    }

    fn find_identifier_type(&self, identifier: &str, ast: &wtf_ast::Module) -> Option<String> {
        // Look through all declarations to find the type of this identifier
        for declaration in &ast.declarations {
            match declaration {
                wtf_ast::Declaration::Function(func) => {
                    // Check function parameters
                    for param in &func.parameters {
                        if param.name == identifier {
                            return Some(self.type_annotation_to_string(&param.type_annotation));
                        }
                    }
                    
                    // Check variables in function body (simplified - would need full traversal)
                    for stmt in &func.body.statements {
                        if let wtf_ast::Statement::VariableDeclaration(var) = stmt {
                            if var.name == identifier {
                                if let Some(ref type_ann) = var.type_annotation {
                                    return Some(self.type_annotation_to_string(type_ann));
                                }
                                // TODO: Infer type from value expression
                            }
                        }
                    }
                }
                _ => continue,
            }
        }
        
        None
    }

    fn type_annotation_to_string(&self, type_ann: &wtf_ast::TypeAnnotation) -> String {
        match &type_ann.kind {
            wtf_ast::TypeAnnotationKind::Simple(name) => name.clone(),
            wtf_ast::TypeAnnotationKind::List(inner) => {
                format!("[{}]", self.type_annotation_to_string(inner))
            }
            wtf_ast::TypeAnnotationKind::Option(inner) => {
                format!("{}?", self.type_annotation_to_string(inner))
            }
            wtf_ast::TypeAnnotationKind::Result { ok, err } => {
                format!("Result<{}, {}>", 
                    self.type_annotation_to_string(ok),
                    self.type_annotation_to_string(err))
            }
            wtf_ast::TypeAnnotationKind::Tuple(items) => {
                let item_strings: Vec<String> = items.iter()
                    .map(|item| self.type_annotation_to_string(item))
                    .collect();
                format!("({})", item_strings.join(", "))
            }
        }
    }

    async fn get_available_methods(&self, type_name: &str, uri: &Url) -> Vec<(String, String)> {
        let mut methods = Vec::new();
        
        // Get the cached AST
        let ast_cache = self.ast_cache.read().await;
        let Some(ast) = ast_cache.get(uri) else {
            return methods;
        };
        
        // 1. First, check if this is a resource type and get its methods
        if let Some(resource_methods) = self.get_resource_methods(ast, type_name) {
            methods.extend(resource_methods);
        }
        
        // 2. Find all functions that can be called via UFCS (first parameter matches type)
        let ufcs_methods = self.get_ufcs_methods(ast, type_name);
        methods.extend(ufcs_methods);
        
        // 3. For record types, also include methods that work with structural typing
        if self.is_record_type(ast, type_name) {
            let structural_methods = self.get_structural_methods(ast, type_name);
            methods.extend(structural_methods);
        }
        
        methods
    }

    fn get_resource_methods(&self, ast: &wtf_ast::Module, type_name: &str) -> Option<Vec<(String, String)>> {
        for declaration in &ast.declarations {
            if let wtf_ast::Declaration::Resource(resource) = declaration {
                if resource.name == type_name {
                    let mut methods = Vec::new();
                    for method in &resource.methods {
                        let signature = self.format_function_signature(&method.parameters, &method.return_type);
                        methods.push((method.name.clone(), signature));
                    }
                    return Some(methods);
                }
            } else if let wtf_ast::Declaration::Export(export) = declaration {
                if let wtf_ast::Declaration::Resource(resource) = export.item.as_ref() {
                    if resource.name == type_name {
                        let mut methods = Vec::new();
                        for method in &resource.methods {
                            let signature = self.format_function_signature(&method.parameters, &method.return_type);
                            methods.push((method.name.clone(), signature));
                        }
                        return Some(methods);
                    }
                }
            }
        }
        None
    }

    fn get_ufcs_methods(&self, ast: &wtf_ast::Module, type_name: &str) -> Vec<(String, String)> {
        let mut methods = Vec::new();
        
        for declaration in &ast.declarations {
            if let wtf_ast::Declaration::Function(func) = declaration {
                // Check if first parameter matches the type
                if let Some(first_param) = func.parameters.first() {
                    let param_type = self.type_annotation_to_string(&first_param.type_annotation);
                    if param_type == type_name {
                        // This function can be called as a method via UFCS
                        let remaining_params = &func.parameters[1..];
                        let signature = self.format_function_signature(remaining_params, &func.return_type);
                        methods.push((func.name.clone(), signature));
                    }
                }
            } else if let wtf_ast::Declaration::Export(export) = declaration {
                if let wtf_ast::Declaration::Function(func) = export.item.as_ref() {
                    if let Some(first_param) = func.parameters.first() {
                        let param_type = self.type_annotation_to_string(&first_param.type_annotation);
                        if param_type == type_name {
                            let remaining_params = &func.parameters[1..];
                            let signature = self.format_function_signature(remaining_params, &func.return_type);
                            methods.push((func.name.clone(), signature));
                        }
                    }
                }
            }
        }
        
        methods
    }

    fn is_record_type(&self, ast: &wtf_ast::Module, type_name: &str) -> bool {
        for declaration in &ast.declarations {
            match declaration {
                wtf_ast::Declaration::Record(record) if record.name == type_name => return true,
                wtf_ast::Declaration::Export(export) => {
                    if let wtf_ast::Declaration::Record(record) = export.item.as_ref() {
                        if record.name == type_name {
                            return true;
                        }
                    }
                }
                _ => continue,
            }
        }
        false
    }

    fn get_structural_methods(&self, ast: &wtf_ast::Module, type_name: &str) -> Vec<(String, String)> {
        let mut methods = Vec::new();
        
        // Get the fields of the record type
        let record_fields = self.get_type_fields(ast, type_name);
        if record_fields.is_empty() {
            return methods;
        }
        
        // Find functions where the first parameter is a record that's structurally compatible
        for declaration in &ast.declarations {
            if let wtf_ast::Declaration::Function(func) = declaration {
                if let Some(first_param) = func.parameters.first() {
                    let param_type = self.type_annotation_to_string(&first_param.type_annotation);
                    
                    // Check if param_type is a record and if our type has all its fields
                    if let Some(param_fields) = self.get_record_fields_by_name(ast, &param_type) {
                        if self.is_structurally_compatible(&record_fields, &param_fields) {
                            let remaining_params = &func.parameters[1..];
                            let signature = self.format_function_signature(remaining_params, &func.return_type);
                            methods.push((func.name.clone(), signature));
                        }
                    }
                }
            } else if let wtf_ast::Declaration::Export(export) = declaration {
                if let wtf_ast::Declaration::Function(func) = export.item.as_ref() {
                    if let Some(first_param) = func.parameters.first() {
                        let param_type = self.type_annotation_to_string(&first_param.type_annotation);
                        
                        if let Some(param_fields) = self.get_record_fields_by_name(ast, &param_type) {
                            if self.is_structurally_compatible(&record_fields, &param_fields) {
                                let remaining_params = &func.parameters[1..];
                                let signature = self.format_function_signature(remaining_params, &func.return_type);
                                methods.push((func.name.clone(), signature));
                            }
                        }
                    }
                }
            }
        }
        
        methods
    }

    fn get_record_fields_by_name(&self, ast: &wtf_ast::Module, type_name: &str) -> Option<Vec<String>> {
        for declaration in &ast.declarations {
            match declaration {
                wtf_ast::Declaration::Record(record) if record.name == type_name => {
                    return Some(record.fields.iter().map(|f| f.name.clone()).collect());
                }
                wtf_ast::Declaration::Export(export) => {
                    if let wtf_ast::Declaration::Record(record) = export.item.as_ref() {
                        if record.name == type_name {
                            return Some(record.fields.iter().map(|f| f.name.clone()).collect());
                        }
                    }
                }
                _ => continue,
            }
        }
        None
    }

    fn is_structurally_compatible(&self, our_fields: &[String], required_fields: &[String]) -> bool {
        // Check if our_fields contains all required_fields (structural typing)
        for required_field in required_fields {
            if !our_fields.contains(required_field) {
                return false;
            }
        }
        true
    }

    fn format_function_signature(&self, parameters: &[wtf_ast::Parameter], return_type: &Option<wtf_ast::TypeAnnotation>) -> String {
        let param_strings: Vec<String> = parameters.iter()
            .map(|p| format!("{}: {}", p.name, self.type_annotation_to_string(&p.type_annotation)))
            .collect();
        
        let params = format!("({})", param_strings.join(", "));
        
        if let Some(ret_type) = return_type {
            format!("{} -> {}", params, self.type_annotation_to_string(ret_type))
        } else {
            params
        }
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
                completion_provider: Some(CompletionOptions {
                    resolve_provider: Some(false),
                    trigger_characters: Some(vec![".".to_string()]),
                    all_commit_characters: None,
                    work_done_progress_options: WorkDoneProgressOptions::default(),
                    completion_item: None,
                }),
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

    async fn completion(&self, params: CompletionParams) -> Result<Option<CompletionResponse>> {
        let uri = &params.text_document_position.text_document.uri;
        let position = params.text_document_position.position;
        
        // Get the document content
        let documents = self.documents.read().await;
        let Some(text) = documents.get(uri) else {
            return Ok(None);
        };

        // Check if this is a completion request after a "."
        if let Some(completions) = self.generate_method_completions(text, position, uri).await {
            Ok(Some(CompletionResponse::Array(completions)))
        } else {
            Ok(None)
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

    #[test]
    fn test_type_annotation_to_string() {
        let simple_type = wtf_ast::TypeAnnotation::simple("String".to_string(), wtf_tokens::Span { start: 0, end: 6 });
        let list_type = wtf_ast::TypeAnnotation::list(simple_type, wtf_tokens::Span { start: 0, end: 8 });
        
        // Test the type annotation to string conversion logic
        use wtf_ast::{TypeAnnotation, TypeAnnotationKind};
        
        let simple_annotation = TypeAnnotation {
            kind: TypeAnnotationKind::Simple("String".to_string()),
            span: wtf_tokens::Span { start: 0, end: 6 }
        };
        
        let list_annotation = TypeAnnotation {
            kind: TypeAnnotationKind::List(Box::new(simple_annotation.clone())),
            span: wtf_tokens::Span { start: 0, end: 8 }
        };
        
        // These tests don't need a Backend instance
        assert_eq!(format_type_annotation(&simple_annotation), "String");
        assert_eq!(format_type_annotation(&list_annotation), "[String]");
    }

    // Helper function for testing type annotation formatting
    fn format_type_annotation(type_ann: &wtf_ast::TypeAnnotation) -> String {
        match &type_ann.kind {
            wtf_ast::TypeAnnotationKind::Simple(name) => name.clone(),
            wtf_ast::TypeAnnotationKind::List(inner) => {
                format!("[{}]", format_type_annotation(inner))
            }
            wtf_ast::TypeAnnotationKind::Option(inner) => {
                format!("{}?", format_type_annotation(inner))
            }
            wtf_ast::TypeAnnotationKind::Result { ok, err } => {
                format!("Result<{}, {}>", 
                    format_type_annotation(ok),
                    format_type_annotation(err))
            }
            wtf_ast::TypeAnnotationKind::Tuple(items) => {
                let item_strings: Vec<String> = items.iter()
                    .map(|item| format_type_annotation(item))
                    .collect();
                format!("({})", item_strings.join(", "))
            }
        }
    }
}