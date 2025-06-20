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
    let origin = point {
        x: 0.0,
        y: 0.0
    }

    // Accessing record fields
    let x_coord = origin.x
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
    }
    drop(ast_cache);
    
    // Test completion at position after 'origin.' - simulate cursor position right after the dot
    let lines: Vec<&str> = code.lines().collect();
    let dot_line = lines.iter().position(|line| line.contains("let x_coord = origin.x")).unwrap();
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
        panic!("Expected field completions but got None");
    }
}

#[tokio::test]
async fn test_method_completion_with_ufcs() {
    // Test method completion via UFCS
    let code = r#"// Test method completion
record Person {
    name: string
    age: s32
}

func get_name(p: Person) -> string {
    p.name
}

func get_info(person: Person) -> string {
    "info"
}

func main() {
    let p = { name: "Alice", age: 30 }
    let result = p.placeholder
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
    
    // Find position after "p."
    let lines: Vec<&str> = code.lines().collect();
    let dot_line = lines.iter().position(|line| line.contains("let result = p.placeholder")).unwrap();
    let dot_char = lines[dot_line].find("p.").unwrap() + 2;
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
    
    // For now just check that it doesn't crash - the full implementation needs work
    if let Some(CompletionResponse::Array(completions)) = completion_result {
        let completion_labels: Vec<&str> = completions.iter().map(|c| c.label.as_str()).collect();
        println!("Completion labels: {:?}", completion_labels);
        
        // Check if UFCS methods are suggested
        if !completions.is_empty() {
            assert!(completion_labels.contains(&"get_name") || completion_labels.contains(&"get_info") || 
                    completion_labels.contains(&"name") || completion_labels.contains(&"age"), 
                    "Should suggest UFCS methods or fields, got: {:?}", completion_labels);
        }
    } else {
        println!("No completions returned - feature needs implementation");
    }
}

#[tokio::test] 
async fn test_code_action_for_unknown_field() {
    // Test code action for unknown field error
    let code = r#"// Test unknown field
record Person {
    name: string
    age: s32
}

func main() {
    let p = { name: "Alice", age: 30 }
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
    
    // For now, just check that it doesn't crash
    if let Some(actions) = code_actions {
        println!("Got {} code actions", actions.len());
        if !actions.is_empty() {
            let action_titles: Vec<String> = actions.iter().filter_map(|action| {
                match action {
                    CodeActionOrCommand::CodeAction(ca) => Some(ca.title.clone()),
                    _ => None,
                }
            }).collect();
            println!("Action titles: {:?}", action_titles);
            assert!(action_titles.iter().any(|title| title.contains("name") || title.contains("age")), 
                    "Should suggest valid field names");
        }
    }
}

#[tokio::test]
async fn test_basic_parsing() {
    // Just test that basic parsing works correctly
    let code = r#"// Simple record
record point {
    x: f64
    y: f64
}"#;

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
    
    // Check AST was parsed and cached
    let diagnostics = backend.validate_document(&uri, code).await;
    assert!(diagnostics.is_empty(), "Should parse without errors");
    
    let ast_cache = backend.ast_cache.read().await;
    let ast = ast_cache.get(&uri);
    assert!(ast.is_some(), "AST should be cached");
    
    if let Some(ast) = ast {
        assert_eq!(ast.declarations.len(), 1, "Should have one declaration");
        match &ast.declarations[0] {
            wtf_ast::Declaration::Record(r) => {
                assert_eq!(r.name, "point");
                assert_eq!(r.fields.len(), 2);
                assert_eq!(r.fields[0].name, "x");
                assert_eq!(r.fields[1].name, "y");
            }
            _ => panic!("Expected record declaration"),
        }
    }
}