use tower_lsp::lsp_types::*;
use tower_lsp::{LspService, LanguageServer};
use wtf_lsp::Backend;

/// Additional tests to cover edge cases and ensure comprehensive functionality
/// These tests complement the main completion_test.rs file

#[tokio::test]
async fn test_diagnostics_comprehensive() {
    let code = r#"
// Test various error types
func test_errors() {
    let x: string = 42        // Type mismatch
    let y = undefined_var     // Undefined variable
    invalid_syntax$           // Syntax error
}

// Test valid syntax for comparison
func valid_function() -> i32 {
    let x = 5
    return x
}
"#;

    let (service, _socket) = LspService::new(|client| Backend::new(client));
    let backend = service.inner();
    let uri = Url::parse("file:///test_diagnostics.wtf").unwrap();
    
    let did_open_params = DidOpenTextDocumentParams {
        text_document: TextDocumentItem {
            uri: uri.clone(),
            language_id: "wtf".to_string(),
            version: 1,
            text: code.to_string(),
        },
    };
    backend.did_open(did_open_params).await;
    
    let diagnostics = backend.validate_document(&uri, code).await;
    
    // Should have multiple diagnostics for various errors
    assert!(!diagnostics.is_empty(), "Should detect multiple error types");
    
    // All diagnostics should be ERROR severity
    for diagnostic in &diagnostics {
        assert_eq!(diagnostic.severity, Some(DiagnosticSeverity::ERROR), 
                   "All WTF errors should be ERROR severity");
        assert_eq!(diagnostic.source, Some("wtf-lsp".to_string()), 
                   "All diagnostics should be attributed to wtf-lsp");
    }
    
    // Verify position mapping is working
    for diagnostic in &diagnostics {
        assert!(diagnostic.range.start.line < 20, "Line number should be reasonable");
        assert!(diagnostic.range.start.character < 100, "Character position should be reasonable");
    }
}

#[tokio::test]
async fn test_keyword_completion_top_level() {
    let code = r#"
// Test top-level keyword completion
// Cursor should be at beginning of line after comment

"#;

    let (service, _socket) = LspService::new(|client| Backend::new(client));
    let backend = service.inner();
    let uri = Url::parse("file:///test_keywords.wtf").unwrap();
    
    let did_open_params = DidOpenTextDocumentParams {
        text_document: TextDocumentItem {
            uri: uri.clone(),
            language_id: "wtf".to_string(),
            version: 1,
            text: code.to_string(),
        },
    };
    backend.did_open(did_open_params).await;
    
    // Test completion at the end of the file (top-level context)
    let lines: Vec<&str> = code.lines().collect();
    let position = Position::new(lines.len() as u32, 0);
    
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
    
    if let Some(CompletionResponse::Array(completions)) = completion_result {
        let keyword_completions: Vec<&CompletionItem> = completions.iter()
            .filter(|c| c.kind == Some(CompletionItemKind::KEYWORD))
            .collect();
        
        println!("Keyword completions: {:?}", keyword_completions);
        
        // Should suggest top-level keywords
        let labels: Vec<&str> = keyword_completions.iter().map(|c| c.label.as_str()).collect();
        assert!(labels.contains(&"func"), "Should suggest 'func' keyword: {:?}", labels);
        assert!(labels.contains(&"record"), "Should suggest 'record' keyword: {:?}", labels);
        
        // Check that func completion includes snippet
        let func_completion = keyword_completions.iter()
            .find(|c| c.label == "func");
        if let Some(completion) = func_completion {
            assert!(completion.insert_text.is_some(), "func completion should have insert text");
            if let Some(insert_text) = &completion.insert_text {
                assert!(insert_text.contains("${"), "func completion should be a snippet");
            }
        }
    } else {
        // It's okay if no completions at top level in some contexts
        println!("No keyword completions returned");
    }
}

#[tokio::test]
async fn test_assignment_error_fixes() {
    let code = r#"
func test_assignment_errors() {
    let x = 5
    x = 10  // Assignment to immutable variable
    
    let y = 20
    y = 30  // Another assignment to immutable
}
"#;

    let (service, _socket) = LspService::new(|client| Backend::new(client));
    let backend = service.inner();
    let uri = Url::parse("file:///test_assignment.wtf").unwrap();
    
    let did_open_params = DidOpenTextDocumentParams {
        text_document: TextDocumentItem {
            uri: uri.clone(),
            language_id: "wtf".to_string(),
            version: 1,
            text: code.to_string(),
        },
    };
    backend.did_open(did_open_params).await;
    
    let diagnostics = backend.validate_document(&uri, code).await;
    
    // Find the first assignment line
    let lines: Vec<&str> = code.lines().collect();
    let assignment_line = lines.iter().position(|line| line.contains("x = 10")).unwrap();
    let assignment_range = Range::new(
        Position::new(assignment_line as u32, 4), // Start at 'x'
        Position::new(assignment_line as u32, lines[assignment_line].len() as u32)
    );
    
    let code_action_params = CodeActionParams {
        text_document: TextDocumentIdentifier { uri: uri.clone() },
        range: assignment_range,
        context: CodeActionContext {
            diagnostics: diagnostics.clone(),
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
        
        println!("Assignment fix actions: {:?}", action_titles);
        
        // Should offer shadowing fix
        assert!(action_titles.iter().any(|title| 
                title.contains("variable declaration") || title.contains("shadow")), 
                "Should offer shadowing fix: {:?}", action_titles);
        
        // Should offer mutability fix
        assert!(action_titles.iter().any(|title| 
                title.contains("mutable") && title.contains("'x'")), 
                "Should offer mutability fix for 'x': {:?}", action_titles);
    } else {
        println!("No assignment fix actions returned");
    }
}

#[tokio::test]
async fn test_edge_cases_and_error_handling() {
    let (service, _socket) = LspService::new(|client| Backend::new(client));
    let backend = service.inner();
    
    // Test 1: Empty document
    let empty_uri = Url::parse("file:///empty.wtf").unwrap();
    let empty_code = "";
    
    let did_open_params = DidOpenTextDocumentParams {
        text_document: TextDocumentItem {
            uri: empty_uri.clone(),
            language_id: "wtf".to_string(),
            version: 1,
            text: empty_code.to_string(),
        },
    };
    backend.did_open(did_open_params).await;
    
    // Should handle empty document gracefully
    let diagnostics = backend.validate_document(&empty_uri, empty_code).await;
    // Empty document may or may not have diagnostics, but shouldn't crash
    
    let completion_params = CompletionParams {
        text_document_position: TextDocumentPositionParams {
            text_document: TextDocumentIdentifier { uri: empty_uri.clone() },
            position: Position::new(0, 0),
        },
        work_done_progress_params: WorkDoneProgressParams::default(),
        partial_result_params: PartialResultParams::default(),
        context: None,
    };
    
    let completion_result = backend.completion(completion_params).await;
    // Should return something (even if empty) and not crash
    assert!(completion_result.is_ok());
    
    // Test 2: Malformed document
    let malformed_uri = Url::parse("file:///malformed.wtf").unwrap();
    let malformed_code = r#"
func broken( {
    let x = 
    invalid $ tokens
}
"#;
    
    let did_open_params = DidOpenTextDocumentParams {
        text_document: TextDocumentItem {
            uri: malformed_uri.clone(),
            language_id: "wtf".to_string(),
            version: 1,
            text: malformed_code.to_string(),
        },
    };
    backend.did_open(did_open_params).await;
    
    // Should handle malformed syntax gracefully
    let diagnostics = backend.validate_document(&malformed_uri, malformed_code).await;
    assert!(!diagnostics.is_empty(), "Should detect syntax errors");
    
    // Should still attempt to provide completions (may be empty)
    let completion_params = CompletionParams {
        text_document_position: TextDocumentPositionParams {
            text_document: TextDocumentIdentifier { uri: malformed_uri.clone() },
            position: Position::new(2, 10), // Somewhere in middle
        },
        work_done_progress_params: WorkDoneProgressParams::default(),
        partial_result_params: PartialResultParams::default(),
        context: None,
    };
    
    let completion_result = backend.completion(completion_params).await;
    assert!(completion_result.is_ok(), "Should not crash on malformed syntax");
    
    // Test 3: Out of bounds position
    let valid_uri = Url::parse("file:///valid.wtf").unwrap();
    let valid_code = "func main() {}";
    
    let did_open_params = DidOpenTextDocumentParams {
        text_document: TextDocumentItem {
            uri: valid_uri.clone(),
            language_id: "wtf".to_string(),
            version: 1,
            text: valid_code.to_string(),
        },
    };
    backend.did_open(did_open_params).await;
    
    // Test position beyond end of document
    let completion_params = CompletionParams {
        text_document_position: TextDocumentPositionParams {
            text_document: TextDocumentIdentifier { uri: valid_uri },
            position: Position::new(100, 100), // Way beyond end
        },
        work_done_progress_params: WorkDoneProgressParams::default(),
        partial_result_params: PartialResultParams::default(),
        context: None,
    };
    
    let completion_result = backend.completion(completion_params).await;
    assert!(completion_result.is_ok(), "Should handle out-of-bounds position gracefully");
}

#[tokio::test]
async fn test_unicode_and_special_characters() {
    let code = r#"
// Test Unicode support in identifiers and strings
record 坐标点 {
    x坐标: f64,
    y坐标: f64,
    名称: string
}

func 测试函数() {
    let 点 = {
        x坐标: 1.0,
        y坐标: 2.0,
        名称: "测试点"
    }
    
    // Test completion on Unicode identifiers
    let 值 = 点.x坐标
}
"#;

    let (service, _socket) = LspService::new(|client| Backend::new(client));
    let backend = service.inner();
    let uri = Url::parse("file:///unicode_test.wtf").unwrap();
    
    let did_open_params = DidOpenTextDocumentParams {
        text_document: TextDocumentItem {
            uri: uri.clone(),
            language_id: "wtf".to_string(),
            version: 1,
            text: code.to_string(),
        },
    };
    backend.did_open(did_open_params).await;
    
    // Test parsing of Unicode document
    let diagnostics = backend.validate_document(&uri, code).await;
    println!("Unicode diagnostics: {:?}", diagnostics);
    
    // Test completion with Unicode identifiers
    let lines: Vec<&str> = code.lines().collect();
    let dot_line = lines.iter().position(|line| line.contains("点.x坐标")).unwrap();
    let dot_char = lines[dot_line].find("点.").unwrap() + "点.".len();
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
    
    if let Some(CompletionResponse::Array(completions)) = completion_result {
        let completion_labels: Vec<&str> = completions.iter().map(|c| c.label.as_str()).collect();
        println!("Unicode completion labels: {:?}", completion_labels);
        
        // Should include Unicode field names
        assert!(completion_labels.contains(&"x坐标"), "Should suggest Unicode field 'x坐标': {:?}", completion_labels);
        assert!(completion_labels.contains(&"y坐标"), "Should suggest Unicode field 'y坐标': {:?}", completion_labels);
        assert!(completion_labels.contains(&"名称"), "Should suggest Unicode field '名称': {:?}", completion_labels);
    } else {
        println!("No completions for Unicode test");
    }
}

#[tokio::test]
async fn test_complex_type_scenarios() {
    let code = r#"
// Test complex type inference scenarios
record Person {
    name: string,
    age: i32,
    address: Address
}

record Address {
    street: string,
    city: string,
    country: string
}

record Employee {
    name: string,
    age: i32,
    department: string,
    salary: f64
}

func process_person(p: Person) -> string {
    return p.name
}

func process_employee(e: Employee) -> string {
    return e.name
}

func test_complex_types() {
    // Anonymous record with fields from multiple types
    let mixed_record = {
        name: "John",
        age: 30,
        department: "Engineering",
        city: "Seattle"
    }
    
    // Should get completions for actual fields
    let test1 = mixed_record.name
    
    // Should get method completions based on structural typing
    let test2 = mixed_record.process_person  // Should work (has name, age)
    let test3 = mixed_record.process_employee // Should work (has name, age)
}
"#;

    let (service, _socket) = LspService::new(|client| Backend::new(client));
    let backend = service.inner();
    let uri = Url::parse("file:///complex_types.wtf").unwrap();
    
    let did_open_params = DidOpenTextDocumentParams {
        text_document: TextDocumentItem {
            uri: uri.clone(),
            language_id: "wtf".to_string(),
            version: 1,
            text: code.to_string(),
        },
    };
    backend.did_open(did_open_params).await;
    
    let diagnostics = backend.validate_document(&uri, code).await;
    println!("Complex type diagnostics: {:?}", diagnostics);
    
    // Test field completion on complex anonymous record
    let lines: Vec<&str> = code.lines().collect();
    let dot_line = lines.iter().position(|line| line.contains("mixed_record.name")).unwrap();
    let dot_char = lines[dot_line].find("mixed_record.").unwrap() + "mixed_record.".len();
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
    
    if let Some(CompletionResponse::Array(completions)) = completion_result {
        let field_completions: Vec<&CompletionItem> = completions.iter()
            .filter(|c| c.kind == Some(CompletionItemKind::FIELD))
            .collect();
        
        let method_completions: Vec<&CompletionItem> = completions.iter()
            .filter(|c| c.kind == Some(CompletionItemKind::FUNCTION))
            .collect();
        
        println!("Complex type field completions: {:?}", field_completions);
        println!("Complex type method completions: {:?}", method_completions);
        
        // Should include all fields from the anonymous record
        let field_labels: Vec<&str> = field_completions.iter().map(|c| c.label.as_str()).collect();
        assert!(field_labels.contains(&"name"), "Should suggest 'name' field");
        assert!(field_labels.contains(&"age"), "Should suggest 'age' field");
        assert!(field_labels.contains(&"department"), "Should suggest 'department' field");
        assert!(field_labels.contains(&"city"), "Should suggest 'city' field");
        
        // Should include methods that are structurally compatible
        let method_labels: Vec<&str> = method_completions.iter().map(|c| c.label.as_str()).collect();
        assert!(method_labels.contains(&"process_person"), "Should suggest 'process_person' method via structural typing");
        assert!(method_labels.contains(&"process_employee"), "Should suggest 'process_employee' method via structural typing");
    }
}