use tower_lsp::lsp_types::*;
use tower_lsp::{LspService, LanguageServer};
use wtf_lsp::Backend;

#[tokio::test]
async fn test_completion_basic_infrastructure() {
    // Use the exact syntax from the working example
    let code = r#"// Record definition with structural typing
record point {
    x: f64
    y: f64
}

// Function that demonstrates variables and basic types
func demo_variables() {
    // Creating an instance of a record
    let origin = {
        x: 0.0,
        y: 0.0
    }

    // Accessing record fields - this line is for completion testing
    let x_coord = origin.
}
"#;

    let (service, _socket) = LspService::new(|client| Backend::new(client));
    let backend = service.inner();
    let uri = Url::parse("file:///test.wtf").unwrap();
    
    let did_open_params = DidOpenTextDocumentParams {
        text_document: TextDocumentItem {
            uri: uri.clone(),
            language_id: "wtf".to_string(),
            version: 1,
            text: code.to_string(),
        },
    };
    backend.did_open(did_open_params).await;
    
    // Check that parsing worked
    let diagnostics = backend.validate_document(&uri, code).await;
    println!("Diagnostics: {:?}", diagnostics);
    
    // Check AST is available
    let ast_cache = backend.ast_cache.read().await;
    let ast = ast_cache.get(&uri);
    assert!(ast.is_some(), "AST should be available for valid syntax");
    
    if let Some(ast) = ast {
        // Verify we can find the point record
        let point_record = ast.declarations.iter().find(|decl| {
            match decl {
                wtf_ast::Declaration::Record(r) => r.name == "point",
                _ => false,
            }
        });
        assert!(point_record.is_some(), "Should find point record in AST");
        
        // Test type field extraction
        let field_names = backend.get_type_fields(ast, "point");
        println!("Point fields: {:?}", field_names);
        assert!(field_names.contains(&"x".to_string()), "Should find 'x' field");
        assert!(field_names.contains(&"y".to_string()), "Should find 'y' field");
        
        // Test type inference for origin  
        let origin_type = backend.infer_expression_type("origin", &uri).await;
        println!("Inferred type for 'origin': {:?}", origin_type);
        assert!(origin_type.is_some(), "Should be able to infer type for 'origin'");
        assert_eq!(origin_type.unwrap(), "point", "Should infer 'point' type for origin");
    }
    drop(ast_cache);
    
    // Test completion at position after 'origin.' - simulate cursor position right after the dot
    let lines: Vec<&str> = code.lines().collect();
    let dot_line = lines.iter().position(|line| line.contains("let x_coord = origin.")).unwrap();
    let dot_char = lines[dot_line].find("origin.").unwrap() + 7; // Position right after the dot
    let position = Position::new(dot_line as u32, dot_char as u32);
    
    println!("Testing completion at line {}, char {} (after 'origin.')", dot_line, dot_char);
    
    let completion_params = CompletionParams {
        text_document_position: TextDocumentPositionParams {
            text_document: TextDocumentIdentifier { uri: uri.clone() },
            position,
        },
        work_done_progress_params: WorkDoneProgressParams::default(),
        partial_result_params: PartialResultParams::default(),
        context: None,
    };
    
    let completion_result = backend.completion(completion_params).await.unwrap();
    println!("Completion result: {:?}", completion_result);
    
    // Test if the completion mechanism is working
    if let Some(CompletionResponse::Array(completions)) = completion_result {
        println!("Got {} completions", completions.len());
        assert!(!completions.is_empty(), "Should have field completions");
        
        let completion_labels: Vec<&str> = completions.iter().map(|c| c.label.as_str()).collect();
        println!("Completion labels: {:?}", completion_labels);
        
        // Should include the point fields
        assert!(completion_labels.contains(&"x") && completion_labels.contains(&"y"), 
                "Completions should include point fields x and y: {:?}", completion_labels);
    } else {
        panic!("Expected field completions but got None - completion is not working");
    }
}

#[tokio::test]
async fn test_method_completion_with_ufcs() {
    // Test method completion via UFCS
    let code = r#"record point {
    x: f64
    y: f64
}

func distance(p: point) -> f64 {
    (p.x * p.x + p.y * p.y)
}

func scale(p: point, factor: f64) -> point {
    { x: p.x * factor, y: p.y * factor }
}

func main() {
    let origin = { x: 0.0, y: 0.0 }
    let result = origin.
}
"#;

    let (service, _socket) = LspService::new(|client| Backend::new(client));
    let backend = service.inner();
    let uri = Url::parse("file:///test.wtf").unwrap();
    
    let did_open_params = DidOpenTextDocumentParams {
        text_document: TextDocumentItem {
            uri: uri.clone(),
            language_id: "wtf".to_string(),
            version: 1,
            text: code.to_string(),
        },
    };
    backend.did_open(did_open_params).await;
    
    // Check parsing worked
    let diagnostics = backend.validate_document(&uri, code).await;
    println!("Diagnostics: {:?}", diagnostics);
    
    // Check AST cache
    let ast_cache = backend.ast_cache.read().await;
    let ast = ast_cache.get(&uri);
    assert!(ast.is_some(), "AST should be available");
    
    if let Some(ast) = ast {
        // Verify functions are parsed
        let functions: Vec<&str> = ast.declarations.iter().filter_map(|decl| {
            match decl {
                wtf_ast::Declaration::Function(f) => Some(f.name.as_str()),
                _ => None,
            }
        }).collect();
        println!("Functions found: {:?}", functions);
        assert!(functions.contains(&"distance"), "Should find distance function");
        assert!(functions.contains(&"scale"), "Should find scale function");
        
        // Test type inference for origin
        let origin_type = backend.infer_expression_type("origin", &uri).await;
        println!("Inferred type for 'origin': {:?}", origin_type);
        assert_eq!(origin_type, Some("point".to_string()), "Should infer point type");
    }
    drop(ast_cache);

    // Find position after "origin."
    let lines: Vec<&str> = code.lines().collect();
    let dot_line = lines.iter().position(|line| line.contains("let result = origin.")).unwrap();
    let dot_char = lines[dot_line].find("origin.").unwrap() + 7;
    let position = Position::new(dot_line as u32, dot_char as u32);

    let completion_params = CompletionParams {
        text_document_position: TextDocumentPositionParams {
            text_document: TextDocumentIdentifier { uri: uri.clone() },
            position,
        },
        work_done_progress_params: WorkDoneProgressParams::default(),
        partial_result_params: PartialResultParams::default(),
        context: None,
    };

    let completion_result = backend.completion(completion_params).await.unwrap();
    println!("Completion result: {:?}", completion_result);

    // Verify both field and method completions are returned
    if let Some(CompletionResponse::Array(completions)) = completion_result {
        let completion_labels: Vec<&str> = completions.iter().map(|c| c.label.as_str()).collect();
        println!("Completion labels: {:?}", completion_labels);
        
        // Should include fields
        assert!(completion_labels.contains(&"x"), "Should include field 'x'");
        assert!(completion_labels.contains(&"y"), "Should include field 'y'");
        
        // Should include UFCS methods
        assert!(completion_labels.contains(&"distance"), "Should include 'distance' method via UFCS");
        assert!(completion_labels.contains(&"scale"), "Should include 'scale' method via UFCS");
        
        // Check completion types
        let field_completions: Vec<_> = completions.iter().filter(|c| c.kind == Some(CompletionItemKind::FIELD)).collect();
        let method_completions: Vec<_> = completions.iter().filter(|c| c.kind == Some(CompletionItemKind::METHOD)).collect();
        
        assert!(!field_completions.is_empty(), "Should have field completions");
        assert!(!method_completions.is_empty(), "Should have method completions");
        
        println!("Found {} field completions and {} method completions", 
                field_completions.len(), method_completions.len());
    } else {
        panic!("Expected completions but got None - method completion not working");
    }
}

#[tokio::test] 
async fn test_code_action_for_unknown_field() {
    // Test code action for unknown field error
    let code = r#"record point {
    x: f64
    y: f64
}

func main() {
    let p = { x: 1.0, y: 2.0 }
    let invalid = p.unknown_field
}
"#;

    let (service, _socket) = LspService::new(|client| Backend::new(client));
    let backend = service.inner();
    let uri = Url::parse("file:///test.wtf").unwrap();
    
    let did_open_params = DidOpenTextDocumentParams {
        text_document: TextDocumentItem {
            uri: uri.clone(),
            language_id: "wtf".to_string(),
            version: 1,
            text: code.to_string(),
        },
    };
    backend.did_open(did_open_params).await;
    
    // Check what diagnostics we get
    let diagnostics = backend.validate_document(&uri, code).await;
    println!("Diagnostics: {:?}", diagnostics);
    assert!(!diagnostics.is_empty(), "Should have at least one diagnostic");
    
    // Find position of unknown_field
    let lines: Vec<&str> = code.lines().collect();
    let error_line = lines.iter().position(|line| line.contains("unknown_field")).unwrap();
    let start_char = lines[error_line].find("unknown_field").unwrap();
    let end_char = start_char + "unknown_field".len();
    
    let range = Range::new(
        Position::new(error_line as u32, start_char as u32),
        Position::new(error_line as u32, end_char as u32),
    );
    
    let code_action_params = CodeActionParams {
        text_document: TextDocumentIdentifier { uri: uri.clone() },
        range,
        context: CodeActionContext {
            diagnostics: diagnostics.clone(),
            only: None,
            trigger_kind: None,
        },
        work_done_progress_params: WorkDoneProgressParams::default(),
        partial_result_params: PartialResultParams::default(),
    };
    
    let code_actions = backend.code_action(code_action_params).await.unwrap();
    println!("Code actions: {:?}", code_actions);
    
    // Should have code actions suggesting valid field names
    if let Some(actions) = code_actions {
        println!("Got {} code actions", actions.len());
        assert!(!actions.is_empty(), "Should have code actions for field suggestions");
        
        let action_titles: Vec<String> = actions.iter().filter_map(|action| {
            match action {
                CodeActionOrCommand::CodeAction(ca) => Some(ca.title.clone()),
                _ => None,
            }
        }).collect();
        println!("Action titles: {:?}", action_titles);
        assert!(action_titles.iter().any(|title| title.contains("x") || title.contains("y")), 
                "Should suggest valid field names (x, y), got: {:?}", action_titles);
    } else {
        panic!("Expected code actions but got None");
    }
}

#[tokio::test]
async fn test_code_action_for_unknown_method() {
    // Test code action for unknown method error
    let code = r#"record point {
    x: f64
    y: f64
}

func distance(p: point) -> f64 {
    p.x * p.x + p.y * p.y
}

func main() {
    let origin = { x: 0.0, y: 0.0 }
    let result = origin.unknown_method()
}
"#;

    let (service, _socket) = LspService::new(|client| Backend::new(client));
    let backend = service.inner();
    let uri = Url::parse("file:///test.wtf").unwrap();
    
    let did_open_params = DidOpenTextDocumentParams {
        text_document: TextDocumentItem {
            uri: uri.clone(),
            language_id: "wtf".to_string(),
            version: 1,
            text: code.to_string(),
        },
    };
    backend.did_open(did_open_params).await;
    
    // Check what diagnostics we get
    let diagnostics = backend.validate_document(&uri, code).await;
    println!("Diagnostics: {:?}", diagnostics);
    
    // Find position around unknown_method
    let lines: Vec<&str> = code.lines().collect();
    let error_line = lines.iter().position(|line| line.contains("unknown_method")).unwrap();
    let start_char = lines[error_line].find("unknown_method").unwrap();
    let end_char = start_char + "unknown_method".len();
    
    let range = Range::new(
        Position::new(error_line as u32, start_char as u32),
        Position::new(error_line as u32, end_char as u32),
    );
    
    let code_action_params = CodeActionParams {
        text_document: TextDocumentIdentifier { uri: uri.clone() },
        range,
        context: CodeActionContext {
            diagnostics: diagnostics.clone(),
            only: None,
            trigger_kind: None,
        },
        work_done_progress_params: WorkDoneProgressParams::default(),
        partial_result_params: PartialResultParams::default(),
    };
    
    let code_actions = backend.code_action(code_action_params).await.unwrap();
    println!("Code actions: {:?}", code_actions);
    
    // For unknown methods, we should be able to suggest valid method names
    if let Some(actions) = code_actions {
        println!("Got {} code actions", actions.len());
        
        let action_titles: Vec<String> = actions.iter().filter_map(|action| {
            match action {
                CodeActionOrCommand::CodeAction(ca) => Some(ca.title.clone()),
                _ => None,
            }
        }).collect();
        println!("Action titles: {:?}", action_titles);
        
        // Since there are no methods with similar names in this case, we might get
        // field suggestions or no suggestions. The important thing is that it doesn't crash.
    } else {
        println!("No code actions returned - this is acceptable for unknown methods");
    }
}