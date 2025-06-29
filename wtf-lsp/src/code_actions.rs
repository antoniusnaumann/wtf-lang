use std::collections::HashMap;
use tower_lsp::lsp_types::*;

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
        
        // Add UFCS transformation actions (not dependent on diagnostics)
        if let Some(mut ufcs_actions) = self.generate_ufcs_transformation_actions(&params.range, text, uri).await {
            all_actions.append(&mut ufcs_actions);
        }
        
        // Add let/var transformation actions (not dependent on diagnostics)
        if let Some(mut var_actions) = self.generate_variable_mutability_actions(&params.range, text, uri).await {
            all_actions.append(&mut var_actions);
        }
        
        // Add assignment to immutable variable fixes
        if let Some(mut assignment_actions) = self.generate_assignment_to_immutable_actions(&params.range, text, uri).await {
            all_actions.append(&mut assignment_actions);
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
        // Or: "Field 'field_name' does not exist on type '{x: f32, y: f32}' at (start, end)"
        let type_info = if let Some(start) = message.find("on type '") {
            let after_type = &message[start + 9..];
            if let Some(end) = after_type.find("'") {
                Some(&after_type[..end])
            } else {
                None
            }
        } else {
            None
        };

        let Some(type_info) = type_info else {
            return None;
        };

        // Check if this is a record literal type or a named type
        let field_names = if type_info.starts_with('{') && type_info.ends_with('}') {
            // This is a record literal like "{x: f32, y: f32}"
            // Extract field names from the literal
            self.extract_fields_from_type_literal(type_info)
        } else {
            // This is a named type - find it in the AST
            let ast_cache = self.ast_cache.read().await;
            let Some(ast) = ast_cache.get(uri) else {
                return None;
            };
            self.get_type_fields(ast, type_info)
        };

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

    fn extract_fields_from_type_literal(&self, type_literal: &str) -> Vec<String> {
        // Parse "{x: f32, y: f32}" to extract field names ["x", "y"]
        let inner = type_literal.trim_start_matches('{').trim_end_matches('}');
        let mut fields = Vec::new();
        
        for part in inner.split(',') {
            let part = part.trim();
            if let Some(colon_pos) = part.find(':') {
                let field_name = part[..colon_pos].trim();
                fields.push(field_name.to_string());
            }
        }
        
        fields
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

    async fn generate_ufcs_transformation_actions(&self, range: &Range, text: &str, uri: &Url) -> Option<Vec<CodeActionOrCommand>> {
        let chars: Vec<char> = text.chars().collect();
        let line_start = self.find_line_start(range.start.line, &chars);
        let line_end = self.find_line_end(range.start.line, &chars);
        
        if line_end <= line_start {
            return None;
        }
        
        let line_text: String = chars[line_start..line_end].iter().collect();
        let mut actions = Vec::new();
        
        // Check for method call pattern: obj.method(args)
        if let Some(method_call) = self.parse_method_call(&line_text) {
            if let Some(action) = self.create_ufcs_to_function_action(&method_call, range, uri).await {
                actions.push(action);
            }
        }
        
        // Check for function call pattern: function(obj, args) where first arg could be receiver
        if let Some(function_call) = self.parse_function_call(&line_text, uri).await {
            if let Some(action) = self.create_function_to_ufcs_action(&function_call, range, uri).await {
                actions.push(action);
            }
        }
        
        if actions.is_empty() {
            None
        } else {
            Some(actions)
        }
    }

    fn parse_method_call(&self, line_text: &str) -> Option<MethodCallInfo> {
        // Look for pattern: receiver.method(arguments)
        // Find the rightmost pattern that looks like "identifier.method("
        
        let mut best_match = None;
        let mut search_pos = 0;
        
        while let Some(dot_pos) = line_text[search_pos..].find('.') {
            let absolute_dot_pos = search_pos + dot_pos;
            
            // Check if this is followed by a valid identifier and then parentheses
            let after_dot = &line_text[absolute_dot_pos + 1..];
            if let Some(paren_pos) = after_dot.find('(') {
                let method_name = after_dot[..paren_pos].trim();
                if method_name.chars().all(|c| c.is_alphanumeric() || c == '_') && !method_name.is_empty() {
                    // Found a potential method call, extract the receiver
                    let before_dot = &line_text[..absolute_dot_pos];
                    
                    // Extract the rightmost identifier before the dot
                    let receiver = if let Some(space_pos) = before_dot.rfind(char::is_whitespace) {
                        before_dot[space_pos..].trim()
                    } else {
                        before_dot.trim()
                    };
                    
                    // Make sure receiver is a valid identifier
                    if receiver.chars().all(|c| c.is_alphanumeric() || c == '_') && !receiver.is_empty() {
                        // Extract arguments (simple parsing - doesn't handle nested parens perfectly)
                        if let Some(close_paren) = after_dot.rfind(')') {
                            let args_text = &after_dot[paren_pos + 1..close_paren].trim();
                            
                            // Calculate the start position of the receiver
                            let receiver_start = if let Some(space_pos) = before_dot.rfind(char::is_whitespace) {
                                space_pos + 1
                            } else {
                                0
                            };
                            
                            // Calculate the end position (after the closing parenthesis)
                            let expr_end = absolute_dot_pos + 1 + close_paren + 1;
                            
                            best_match = Some(MethodCallInfo {
                                receiver: receiver.to_string(),
                                method_name: method_name.to_string(),
                                arguments: if args_text.is_empty() { 
                                    Vec::new() 
                                } else { 
                                    args_text.split(',').map(|s| s.trim().to_string()).collect() 
                                },
                                full_text: line_text[receiver_start..expr_end].to_string(),
                                start_col: receiver_start,
                                end_col: expr_end,
                            });
                        }
                    }
                }
            }
            
            search_pos = absolute_dot_pos + 1;
        }
        
        best_match
    }

    async fn parse_function_call(&self, line_text: &str, uri: &Url) -> Option<FunctionCallInfo> {
        // Look for pattern: function_name(arg1, arg2, ...)
        // Where arg1 could be used as a receiver for UFCS
        
        // Find the rightmost pattern that looks like "identifier("
        let paren_pos = line_text.rfind('(')?;
        
        // Extract function name (look backwards from the paren to find the identifier)
        let before_paren = &line_text[..paren_pos];
        let function_name = if let Some(space_pos) = before_paren.rfind(char::is_whitespace) {
            before_paren[space_pos..].trim()
        } else {
            before_paren.trim()
        };
        
        // Check if function name is a valid identifier
        if !function_name.chars().all(|c| c.is_alphanumeric() || c == '_') || function_name.is_empty() {
            return None;
        }
        
        // Check if this function exists in the AST
        let ast_cache = self.ast_cache.read().await;
        let ast = ast_cache.get(uri)?;
        
        let function_exists = ast.declarations.iter().any(|decl| {
            matches!(decl, wtf_ast::Declaration::Function(f) if f.name == function_name)
        });
        
        if !function_exists {
            return None;
        }
        
        // Extract arguments
        let close_paren = line_text.rfind(')')?;
        let args_text = &line_text[paren_pos + 1..close_paren].trim();
        let arguments: Vec<String> = if args_text.is_empty() {
            Vec::new()
        } else {
            args_text.split(',').map(|s| s.trim().to_string()).collect()
        };
        
        // Only offer transformation if there's at least one argument to use as receiver
        if arguments.is_empty() {
            return None;
        }
        
        // Calculate the start position of the function name
        let function_start = if let Some(space_pos) = before_paren.rfind(char::is_whitespace) {
            space_pos + 1
        } else {
            0
        };
        
        // End position is after the closing parenthesis
        let expr_end = close_paren + 1;
        
        Some(FunctionCallInfo {
            function_name: function_name.to_string(),
            arguments,
            full_text: line_text[function_start..expr_end].to_string(),
            start_col: function_start,
            end_col: expr_end,
        })
    }

    async fn create_ufcs_to_function_action(&self, method_call: &MethodCallInfo, range: &Range, uri: &Url) -> Option<CodeActionOrCommand> {
        // Transform: receiver.method(args) -> method(receiver, args)
        let mut new_args = vec![method_call.receiver.clone()];
        new_args.extend(method_call.arguments.iter().cloned());
        
        let new_text = if new_args.len() == 1 {
            format!("{}({})", method_call.method_name, new_args[0])
        } else {
            format!("{}({})", method_call.method_name, new_args.join(", "))
        };
        
        // Calculate the proper range for the method call expression
        let expression_range = Range::new(
            Position::new(range.start.line, method_call.start_col as u32),
            Position::new(range.start.line, method_call.end_col as u32)
        );
        
        let text_edit = TextEdit {
            range: expression_range,
            new_text,
        };
        
        Some(CodeActionOrCommand::CodeAction(CodeAction {
            title: format!("Convert to function call: {}(...)", method_call.method_name),
            kind: Some(CodeActionKind::REFACTOR_REWRITE),
            edit: Some(WorkspaceEdit {
                changes: Some({
                    let mut changes = std::collections::HashMap::new();
                    changes.insert(uri.clone(), vec![text_edit]);
                    changes
                }),
                ..Default::default()
            }),
            ..Default::default()
        }))
    }

    async fn create_function_to_ufcs_action(&self, function_call: &FunctionCallInfo, range: &Range, uri: &Url) -> Option<CodeActionOrCommand> {
        // Transform: function(receiver, args) -> receiver.function(args)
        if function_call.arguments.is_empty() {
            return None;
        }
        
        let receiver = &function_call.arguments[0];
        let remaining_args = &function_call.arguments[1..];
        
        let new_text = if remaining_args.is_empty() {
            format!("{}.{}()", receiver, function_call.function_name)
        } else {
            format!("{}.{}({})", receiver, function_call.function_name, remaining_args.join(", "))
        };
        
        // Calculate the proper range for the function call expression
        let expression_range = Range::new(
            Position::new(range.start.line, function_call.start_col as u32),
            Position::new(range.start.line, function_call.end_col as u32)
        );
        
        let text_edit = TextEdit {
            range: expression_range,
            new_text,
        };
        
        Some(CodeActionOrCommand::CodeAction(CodeAction {
            title: format!("Convert to method call: {}.{}(...)", receiver, function_call.function_name),
            kind: Some(CodeActionKind::REFACTOR_REWRITE),
            edit: Some(WorkspaceEdit {
                changes: Some({
                    let mut changes = std::collections::HashMap::new();
                    changes.insert(uri.clone(), vec![text_edit]);
                    changes
                }),
                ..Default::default()
            }),
            ..Default::default()
        }))
    }

    async fn generate_variable_mutability_actions(&self, range: &Range, text: &str, uri: &Url) -> Option<Vec<CodeActionOrCommand>> {
        let chars: Vec<char> = text.chars().collect();
        let line_start = self.find_line_start(range.start.line, &chars);
        let line_end = self.find_line_end(range.start.line, &chars);
        
        if line_end <= line_start {
            return None;
        }
        
        let line_text: String = chars[line_start..line_end].iter().collect();
        let mut actions = Vec::new();
        
        // Check if the line contains a variable declaration with let or var
        if let Some(let_pos) = line_text.find("let ") {
            // Check if the cursor is on or near the let keyword
            let let_line_pos = let_pos as u32;
            if range.start.character <= let_line_pos + 3 && range.end.character >= let_line_pos {
                // Offer to change let to var
                let action = CodeActionOrCommand::CodeAction(CodeAction {
                    title: "Change 'let' to 'var'".to_string(),
                    kind: Some(CodeActionKind::REFACTOR_REWRITE),
                    edit: Some(WorkspaceEdit {
                        changes: Some({
                            let mut changes = HashMap::new();
                            let var_range = Range::new(
                                Position::new(range.start.line, let_line_pos),
                                Position::new(range.start.line, let_line_pos + 3)
                            );
                            changes.insert(uri.clone(), vec![TextEdit {
                                range: var_range,
                                new_text: "var".to_string(),
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
        
        if let Some(var_pos) = line_text.find("var ") {
            // Check if the cursor is on or near the var keyword
            let var_line_pos = var_pos as u32;
            if range.start.character <= var_line_pos + 3 && range.end.character >= var_line_pos {
                // Offer to change var to let
                let action = CodeActionOrCommand::CodeAction(CodeAction {
                    title: "Change 'var' to 'let'".to_string(),
                    kind: Some(CodeActionKind::REFACTOR_REWRITE),
                    edit: Some(WorkspaceEdit {
                        changes: Some({
                            let mut changes = HashMap::new();
                            let let_range = Range::new(
                                Position::new(range.start.line, var_line_pos),
                                Position::new(range.start.line, var_line_pos + 3)
                            );
                            changes.insert(uri.clone(), vec![TextEdit {
                                range: let_range,
                                new_text: "let".to_string(),
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
        
        if actions.is_empty() {
            None
        } else {
            Some(actions)
        }
    }

    async fn generate_assignment_to_immutable_actions(&self, range: &Range, text: &str, uri: &Url) -> Option<Vec<CodeActionOrCommand>> {
        let chars: Vec<char> = text.chars().collect();
        let line_start = self.find_line_start(range.start.line, &chars);
        let line_end = self.find_line_end(range.start.line, &chars);
        
        if line_end <= line_start {
            return None;
        }
        
        let line_text: String = chars[line_start..line_end].iter().collect();
        
        // Look for assignment pattern: identifier = value
        if let Some(equals_pos) = line_text.find(" = ") {
            let before_equals = line_text[..equals_pos].trim();
            
            // Check if this looks like a simple assignment (not a declaration)
            if !before_equals.contains("let") && !before_equals.contains("var") {
                // Extract the variable name
                let var_name = if let Some(space_pos) = before_equals.rfind(char::is_whitespace) {
                    before_equals[space_pos..].trim()
                } else {
                    before_equals
                };
                
                // Check if variable name is valid
                if var_name.chars().all(|c| c.is_alphanumeric() || c == '_') && !var_name.is_empty() {
                    // Look for the original declaration of this variable in the text
                    if let Some(declaration_info) = self.find_variable_declaration(text, var_name) {
                        if declaration_info.is_immutable {
                            let mut actions = Vec::new();
                            
                            // Action 1: Change assignment to variable declaration (shadowing)
                            let shadow_action = CodeActionOrCommand::CodeAction(CodeAction {
                                title: format!("Change assignment to variable declaration (shadow '{}')", var_name),
                                kind: Some(CodeActionKind::QUICKFIX),
                                edit: Some(WorkspaceEdit {
                                    changes: Some({
                                        let mut changes = HashMap::new();
                                        // Calculate the correct range for the assignment line
                                        // We want to replace the entire line content, not the entire file
                                        let assignment_range = Range::new(
                                            Position::new(range.start.line, 0),
                                            Position::new(range.start.line, (line_end - line_start) as u32)
                                        );
                                        
                                        // Extract indentation from the original line and preserve it
                                        let indent_end = line_text.len() - line_text.trim_start().len();
                                        let indentation = &line_text[..indent_end];
                                        let assignment_part = line_text.trim_start();
                                        let new_text = format!("{}let {}", indentation, assignment_part);
                                        
                                        changes.insert(uri.clone(), vec![TextEdit {
                                            range: assignment_range,
                                            new_text,
                                        }]);
                                        changes
                                    }),
                                    ..Default::default()
                                }),
                                ..Default::default()
                            });
                            actions.push(shadow_action);
                            
                            // Action 2: Make original variable mutable
                            let mutable_action = CodeActionOrCommand::CodeAction(CodeAction {
                                title: format!("Make '{}' mutable (change 'let' to 'var')", var_name),
                                kind: Some(CodeActionKind::QUICKFIX),
                                edit: Some(WorkspaceEdit {
                                    changes: Some({
                                        let mut changes = HashMap::new();
                                        let let_range = Range::new(
                                            Position::new(declaration_info.line, declaration_info.let_start_col),
                                            Position::new(declaration_info.line, declaration_info.let_start_col + 3)
                                        );
                                        changes.insert(uri.clone(), vec![TextEdit {
                                            range: let_range,
                                            new_text: "var".to_string(),
                                        }]);
                                        changes
                                    }),
                                    ..Default::default()
                                }),
                                ..Default::default()
                            });
                            actions.push(mutable_action);
                            
                            return Some(actions);
                        }
                    }
                }
            }
        }
        
        None
    }

    fn find_variable_declaration(&self, text: &str, var_name: &str) -> Option<VariableDeclarationInfo> {
        let lines: Vec<&str> = text.lines().collect();
        
        for (line_num, line) in lines.iter().enumerate() {
            // Look for patterns like "let var_name = " or "var var_name = "
            if let Some(let_pos) = line.find("let ") {
                let after_let = &line[let_pos + 4..];
                if let Some(equals_pos) = after_let.find(" = ") {
                    let declared_name = after_let[..equals_pos].trim();
                    if declared_name == var_name {
                        return Some(VariableDeclarationInfo {
                            line: line_num as u32,
                            let_start_col: let_pos as u32,
                            is_immutable: true,
                        });
                    }
                }
            }
            
            if let Some(var_pos) = line.find("var ") {
                let after_var = &line[var_pos + 4..];
                if let Some(equals_pos) = after_var.find(" = ") {
                    let declared_name = after_var[..equals_pos].trim();
                    if declared_name == var_name {
                        return Some(VariableDeclarationInfo {
                            line: line_num as u32,
                            let_start_col: var_pos as u32,
                            is_immutable: false,
                        });
                    }
                }
            }
        }
        
        None
    }


}

#[derive(Debug)]
struct MethodCallInfo {
    receiver: String,
    method_name: String,
    arguments: Vec<String>,
    full_text: String,
    start_col: usize,
    end_col: usize,
}

#[derive(Debug)]
struct FunctionCallInfo {
    function_name: String,
    arguments: Vec<String>,
    full_text: String,
    start_col: usize,
    end_col: usize,
}

#[derive(Debug)]
struct VariableDeclarationInfo {
    line: u32,
    let_start_col: u32,
    is_immutable: bool,
}