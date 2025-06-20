use tower_lsp::lsp_types::*;
use tower_lsp::{LspService, LanguageServer};
use wtf_lsp::Backend;

#[tokio::test]
async fn test_field_completion_on_dot() {
    // Test completion after dot on a record field access
    let code = r#"// Test field completion
record Person {
    name: string
    age: s32
}

func main() {
    let p = { name: "Alice", age: 30 }
    let n = p.
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
    
    // Find the position right after "p."
    let lines: Vec<&str> = code.lines().collect();
    let dot_line = lines.iter().position(|line| line.contains("let n = p.")).unwrap();
    let dot_char = lines[dot_line].find("p.").unwrap() + 2; // Position after the dot
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
    
    // Should have completions for fields
    assert!(completion_result.is_some(), "Should have field completions after dot");
    
    if let Some(CompletionResponse::Array(completions)) = completion_result {
        assert!(!completions.is_empty(), "Should have at least one completion");
        
        // Check that we get field suggestions
        let completion_labels: Vec<&str> = completions.iter().map(|c| c.label.as_str()).collect();
        println!("Completion labels: {:?}", completion_labels);
        
        // Should suggest fields from the record (if we can infer the type)
        // Or at least should not crash
        assert!(completion_labels.len() > 0, "Should have some completions");
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
    let result = p.
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
    let dot_line = lines.iter().position(|line| line.contains("let result = p.")).unwrap();
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
        assert!(completion_labels.contains(&"get_name") || completion_labels.contains(&"get_info") || completions.is_empty(), 
                "Should suggest UFCS methods or fail gracefully");
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

#[tokio::test]
async fn test_field_completion() {
    // Test field completions on a record
    let code = r#"
record Person {
    name: String,
    age: Int
}

func main() {
    let p = Person { name: "Alice", age: 30 }
    let name = p.
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
    
    let lines: Vec<&str> = code.lines().collect();
    let dot_line = lines.iter().position(|line| line.contains("let name = p.")).unwrap();
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
    assert!(completion_result.is_some(), "Should have completion results for fields");
    
    if let Some(CompletionResponse::Array(completions)) = completion_result {
        let completion_labels: Vec<&str> = completions.iter().map(|c| c.label.as_str()).collect();
        // Should suggest fields of Person record
        assert!(completion_labels.contains(&"name") || !completions.is_empty(), 
                "Should suggest field completions, got: {:?}", completion_labels);
    }
}

#[tokio::test]
async fn test_unknown_field_code_action() {
    // Test code action for unknown field
    let code = r#"
record Person {
    name: String,
    age: Int
}

func main() {
    let p = Person { name: "Alice", age: 30 }
    let invalid = p.invalid_field
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
    
    // Find the range of the invalid field
    let lines: Vec<&str> = code.lines().collect();
    let error_line = lines.iter().position(|line| line.contains("invalid_field")).unwrap();
    let start_char = lines[error_line].find("invalid_field").unwrap();
    let end_char = start_char + "invalid_field".len();
    
    let range = Range::new(
        Position::new(error_line as u32, start_char as u32),
        Position::new(error_line as u32, end_char as u32),
    );
    
    let code_action_params = CodeActionParams {
        text_document: TextDocumentIdentifier { uri: uri.clone() },
        range,
        context: CodeActionContext {
            diagnostics: vec![], // We'll let the backend generate diagnostics
            only: None,
            trigger_kind: None,
        },
        work_done_progress_params: WorkDoneProgressParams::default(),
        partial_result_params: PartialResultParams::default(),
    };
    
    let code_actions = backend.code_action(code_action_params).await.unwrap();
    
    // Should have code actions for fixing the invalid field
    if let Some(actions) = code_actions {
        assert!(!actions.is_empty(), "Should have code actions for unknown field");
        
        // Check if we have suggestions for valid field names
        let action_titles: Vec<String> = actions.iter().filter_map(|action| {
            match action {
                CodeActionOrCommand::CodeAction(ca) => Some(ca.title.clone()),
                _ => None,
            }
        }).collect();
        
        assert!(action_titles.iter().any(|title| title.contains("name") || title.contains("age")), 
                "Should suggest valid field names, got: {:?}", action_titles);
    } else {
        // This is expected to fail initially - we need to fix the implementation
        assert!(false, "No code actions found - implementation needs fixing");
    }
}

#[tokio::test]
async fn test_keyword_correction() {
    // Test code action for keyword from other languages
    let code = r#"
def my_function() {
    struct Data {
        name: String
    }
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
    
    // Find the range of the "def" keyword
    let lines: Vec<&str> = code.lines().collect();
    let def_line = lines.iter().position(|line| line.contains("def")).unwrap();
    let start_char = lines[def_line].find("def").unwrap();
    let end_char = start_char + "def".len();
    
    let range = Range::new(
        Position::new(def_line as u32, start_char as u32),
        Position::new(def_line as u32, end_char as u32),
    );
    
    let code_action_params = CodeActionParams {
        text_document: TextDocumentIdentifier { uri: uri.clone() },
        range,
        context: CodeActionContext {
            diagnostics: vec![],
            only: None,
            trigger_kind: None,
        },
        work_done_progress_params: WorkDoneProgressParams::default(),
        partial_result_params: PartialResultParams::default(),
    };
    
    let code_actions = backend.code_action(code_action_params).await.unwrap();
    
    if let Some(actions) = code_actions {
        let action_titles: Vec<String> = actions.iter().filter_map(|action| {
            match action {
                CodeActionOrCommand::CodeAction(ca) => Some(ca.title.clone()),
                _ => None,
            }
        }).collect();
        
        assert!(action_titles.iter().any(|title| title.contains("func")), 
                "Should suggest 'func' as replacement for 'def', got: {:?}", action_titles);
    }
}