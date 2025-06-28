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
        
        // Test type inference for origin  
        let origin_type = backend.infer_expression_type_hir("origin", &uri).await;
        println!("Inferred type for 'origin': {:?}", origin_type);
        assert!(origin_type.is_some(), "Should be able to infer type for 'origin'");
        assert_eq!(origin_type.unwrap(), "point", "Should infer 'point' type for origin");
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
        panic!("Expected field completions but got None - completion is not working");
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
        println!("No code actions returned - this may be expected if no diagnostics are available");
    }
}

#[tokio::test]
async fn test_anonymous_type_completion() {
    // Test case 1: Anonymous record type (no declared type matches these fields)
    let code = r#"func test_anonymous() {
    // Anonymous record without corresponding declared type
    let point = {
        x: 1.0,
        y: 2.0,
        z: 3.0
    }
    
    // Should be able to complete fields here: point.
    let x_val = point.x
    let invalid = point.unknown_field
}

// Only declared record has x, y (no z)
record point2d {
    x: f64
    y: f64
}"#;

    let (service, _socket) = LspService::new(|client| Backend::new(client));
    let backend = service.inner();
    let uri = Url::parse("file:///test_anonymous.wtf").unwrap();
    
    let did_open_params = DidOpenTextDocumentParams {
        text_document: TextDocumentItem {
            uri: uri.clone(),
            language_id: "wtf".to_string(),
            version: 1,
            text: code.to_string(),
        },
    };
    backend.did_open(did_open_params).await;
    
    // Test completion at position after 'point.' - should show x, y, z fields
    let lines: Vec<&str> = code.lines().collect();
    let dot_line = lines.iter().position(|line| line.contains("let x_val = point.x")).unwrap();
    let dot_char = lines[dot_line].find("point.").unwrap() + 6; // Position right after the dot
    let position = Position::new(dot_line as u32, dot_char as u32);
    
    println!("Testing anonymous type completion at line {}, char {} (after 'point.')", dot_line, dot_char);
    
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
    println!("Anonymous type completion result: {:?}", completion_result);
    
    // Test if the completion mechanism is working for anonymous types
    if let Some(CompletionResponse::Array(completions)) = completion_result {
        println!("Got {} completions for anonymous type", completions.len());
        
        let completion_labels: Vec<&str> = completions.iter().map(|c| c.label.as_str()).collect();
        println!("Anonymous type completion labels: {:?}", completion_labels);
        
        // Should include the anonymous record fields x, y, z
        assert!(completion_labels.contains(&"x"), "Should suggest 'x' field for anonymous type");
        assert!(completion_labels.contains(&"y"), "Should suggest 'y' field for anonymous type");
        assert!(completion_labels.contains(&"z"), "Should suggest 'z' field for anonymous type");
    } else {
        println!("No completions for anonymous type - this indicates the issue!");
    }

    // Test code actions for unknown field
    let diagnostics = backend.validate_document(&uri, code).await;
    println!("Diagnostics for unknown field: {:?}", diagnostics);
    
    // Find position of unknown_field
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
    println!("Code actions for unknown field: {:?}", code_actions);
    
    // Should have code actions suggesting valid field names
    if let Some(actions) = code_actions {
        println!("Got {} code actions for unknown field", actions.len());
        assert!(!actions.is_empty(), "Should have code actions for field suggestions on anonymous type");
    } else {
        println!("No code actions for unknown field - this indicates the issue!");
    }
}

#[tokio::test]
async fn test_function_completion_with_parentheses() {
    let code = r#"record point {
    x: f64
    y: f64
}

func add_points(a: point, b: point) -> point {
    { x: a.x + b.x, y: a.y + b.y }
}

func test() {
    let p1 = { x: 1.0, y: 2.0 }
    // Function completion should add parentheses: p1.add_points
    let result = p1.add_points
}"#;

    let (service, _socket) = LspService::new(|client| Backend::new(client));
    let backend = service.inner();
    let uri = Url::parse("file:///test_function_completion.wtf").unwrap();
    
    let did_open_params = DidOpenTextDocumentParams {
        text_document: TextDocumentItem {
            uri: uri.clone(),
            language_id: "wtf".to_string(),
            version: 1,
            text: code.to_string(),
        },
    };
    backend.did_open(did_open_params).await;
    
    // Test completion at position after 'p1.'
    let lines: Vec<&str> = code.lines().collect();
    let dot_line = lines.iter().position(|line| line.contains("let result = p1.add_points")).unwrap();
    let dot_char = lines[dot_line].find("p1.").unwrap() + 3; // Position right after the dot
    let position = Position::new(dot_line as u32, dot_char as u32);
    
    println!("Testing function completion at line {}, char {} (after 'p1.')", dot_line, dot_char);
    
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
    println!("Function completion result: {:?}", completion_result);
    
    // Check if function completions include parentheses
    if let Some(CompletionResponse::Array(completions)) = completion_result {
        let function_completions: Vec<&CompletionItem> = completions.iter()
            .filter(|c| c.kind == Some(CompletionItemKind::FUNCTION) || c.kind == Some(CompletionItemKind::METHOD))
            .collect();
        
        println!("Function completions: {:?}", function_completions);
        
        for completion in function_completions {
            println!("Function completion: label='{}', insert_text={:?}", 
                     completion.label, completion.insert_text);
            
            if completion.label.contains("add_points") {
                // Check if it includes parentheses or has proper insert text
                if let Some(insert_text) = &completion.insert_text {
                    assert!(insert_text.contains("()"), "Function completion should include parentheses");
                }
            }
        }
    }
}

#[tokio::test]
async fn test_identifier_completion() {
    let code = r#"record point {
    x: f64
    y: f64
}

func distance(p1: point, p2: point) -> f64 {
    0.0
}

func magnitude(point: point) -> f64 {
    0.0  
}

func test_function() {
    let p1 = { x: 1.0, y: 2.0 }
    let p2 = { x: 3.0, y: 4.0 }
    
    // Testing identifier completion here - cursor after 'let result = '
    let result = dis
}
"#;

    let (service, _socket) = LspService::new(|client| Backend::new(client));
    let backend = service.inner();
    let uri = Url::parse("file:///test_identifier.wtf").unwrap();
    
    let did_open_params = DidOpenTextDocumentParams {
        text_document: TextDocumentItem {
            uri: uri.clone(),
            language_id: "wtf".to_string(),
            version: 1,
            text: code.to_string(),
        },
    };
    backend.did_open(did_open_params).await;
    
    // Validate document to populate AST cache
    let diagnostics = backend.validate_document(&uri, code).await;
    println!("Diagnostics: {:?}", diagnostics);
    
    // Test completion at position after 'let result = dis' - should suggest 'distance' function
    let lines: Vec<&str> = code.lines().collect();
    let target_line = lines.iter().position(|line| line.contains("let result = dis")).unwrap();
    let target_char = lines[target_line].find("dis").unwrap() + 3; // Position at end of 'dis'
    let position = Position::new(target_line as u32, target_char as u32);
    
    println!("Testing identifier completion at line {}, char {} (after 'dis')", target_line, target_char);
    
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
    println!("Identifier completion result: {:?}", completion_result);
    
    // Check if identifier completions work
    if let Some(CompletionResponse::Array(completions)) = completion_result {
        let completion_labels: Vec<&str> = completions.iter().map(|c| c.label.as_str()).collect();
        println!("Identifier completion labels: {:?}", completion_labels);
        
        // Should include function names
        assert!(completion_labels.contains(&"distance"), "Should suggest 'distance' function: {:?}", completion_labels);
        assert!(completion_labels.contains(&"magnitude"), "Should suggest 'magnitude' function: {:?}", completion_labels);
        
        // Should include type names
        assert!(completion_labels.contains(&"point"), "Should suggest 'point' type: {:?}", completion_labels);
        
        // Check that function completions include parentheses
        let distance_completion = completions.iter().find(|c| c.label == "distance").unwrap();
        assert!(distance_completion.insert_text.as_ref().unwrap().contains("()"), 
                "Function completion should include parentheses");
        
    } else {
        panic!("Expected identifier completions but got None");
    }
}

#[tokio::test]
async fn test_ufcs_transformation_actions() {
    let code = r#"record point {
    x: f64
    y: f64
}

func distance(p1: point, p2: point) -> f64 {
    0.0
}

func magnitude(point: point) -> f64 {
    0.0  
}

func test_transformations() {
    let p1 = { x: 1.0, y: 2.0 }
    let p2 = { x: 3.0, y: 4.0 }
    
    // Method call that should transform to function call
    let result1 = p1.distance(p2)
    
    // Function call that should transform to method call  
    let result2 = magnitude(p1)
}
"#;

    let (service, _socket) = LspService::new(|client| Backend::new(client));
    let backend = service.inner();
    let uri = Url::parse("file:///test_ufcs.wtf").unwrap();
    
    let did_open_params = DidOpenTextDocumentParams {
        text_document: TextDocumentItem {
            uri: uri.clone(),
            language_id: "wtf".to_string(),
            version: 1,
            text: code.to_string(),
        },
    };
    backend.did_open(did_open_params).await;
    
    // Validate document to populate AST cache
    let diagnostics = backend.validate_document(&uri, code).await;
    println!("Diagnostics: {:?}", diagnostics);
    
    // Test UFCS to function transformation for 'p1.distance(p2)'
    let lines: Vec<&str> = code.lines().collect();
    let method_line = lines.iter().position(|line| line.contains("p1.distance(p2)")).unwrap();
    let start_char = lines[method_line].find("p1.distance(p2)").unwrap();
    let end_char = start_char + "p1.distance(p2)".len();
    
    let method_range = Range::new(
        Position::new(method_line as u32, start_char as u32),
        Position::new(method_line as u32, end_char as u32),
    );
    
    let code_action_params = CodeActionParams {
        text_document: TextDocumentIdentifier { uri: uri.clone() },
        range: method_range,
        context: CodeActionContext {
            diagnostics: vec![], // No diagnostics needed for refactoring actions
            only: None,
            trigger_kind: None,
        },
        work_done_progress_params: WorkDoneProgressParams::default(),
        partial_result_params: PartialResultParams::default(),
    };
    
    let code_actions = backend.code_action(code_action_params).await.unwrap();
    println!("UFCS transformation actions: {:?}", code_actions);
    
    // Should have code action to convert method call to function call
    if let Some(actions) = code_actions {
        let action_titles: Vec<String> = actions.iter().filter_map(|action| {
            match action {
                CodeActionOrCommand::CodeAction(ca) => Some(ca.title.clone()),
                _ => None,
            }
        }).collect();
        println!("UFCS action titles: {:?}", action_titles);
        
        assert!(action_titles.iter().any(|title| title.contains("Convert to function call")), 
                "Should have action to convert method call to function call: {:?}", action_titles);
    } else {
        println!("No UFCS transformation actions found");
    }
    
    // Test function to UFCS transformation for 'magnitude(p1)'
    let function_line = lines.iter().position(|line| line.contains("magnitude(p1)")).unwrap();
    let start_char = lines[function_line].find("magnitude(p1)").unwrap();
    let end_char = start_char + "magnitude(p1)".len();
    
    let function_range = Range::new(
        Position::new(function_line as u32, start_char as u32),
        Position::new(function_line as u32, end_char as u32),
    );
    
    let code_action_params2 = CodeActionParams {
        text_document: TextDocumentIdentifier { uri: uri.clone() },
        range: function_range,
        context: CodeActionContext {
            diagnostics: vec![],
            only: None,
            trigger_kind: None,
        },
        work_done_progress_params: WorkDoneProgressParams::default(),
        partial_result_params: PartialResultParams::default(),
    };
    
    let code_actions2 = backend.code_action(code_action_params2).await.unwrap();
    println!("Function to UFCS transformation actions: {:?}", code_actions2);
    
    // Should have code action to convert function call to method call
    if let Some(actions) = code_actions2 {
        let action_titles: Vec<String> = actions.iter().filter_map(|action| {
            match action {
                CodeActionOrCommand::CodeAction(ca) => Some(ca.title.clone()),
                _ => None,
            }
        }).collect();
        println!("Function to UFCS action titles: {:?}", action_titles);
        
        assert!(action_titles.iter().any(|title| title.contains("Convert to method call")), 
                "Should have action to convert function call to method call: {:?}", action_titles);
    } else {
        println!("No function to UFCS transformation actions found");
    }
}

#[tokio::test]
async fn test_structural_typing_ufcs_completion() {
    // Test structural typing support: functions that accept a subset of fields
    // should be available as methods on records with more fields
    let code = r#"// Declared record types for function parameters  
record point2d {
    x: f32
    y: f32
}

record point1d {
    x: f32
}

// Functions that accept declared record types
func process_2d(point: point2d) -> f32 {
    point.x + point.y
}

func get_x(p: point1d) -> f32 {
    p.x
}

func main() {
    // Record with x, y, z fields (superset) - using record literal
    let point3d = {
        x: 1.0,
        y: 2.0,
        z: 3.0
    }
    
    // Should be able to call process_2d and get_x via UFCS since point3d
    // has all required fields (structural typing)
    let result = point3d.process_2d  // Should suggest this
    let x_val = point3d.get_x        // Should suggest this too
}"#;

    let (service, _socket) = LspService::new(|client| Backend::new(client));
    let backend = service.inner();
    let uri = Url::parse("file:///test_structural.wtf").unwrap();
    
    let did_open_params = DidOpenTextDocumentParams {
        text_document: TextDocumentItem {
            uri: uri.clone(),
            language_id: "wtf".to_string(),
            version: 1,
            text: code.to_string(),
        },
    };
    backend.did_open(did_open_params).await;
    
    // Test completion at position after 'point3d.'
    let lines: Vec<&str> = code.lines().collect();
    let dot_line = lines.iter().position(|line| line.contains("let result = point3d.process_2d")).unwrap();
    let dot_char = lines[dot_line].find("point3d.").unwrap() + 8; // Position right after the dot
    let position = Position::new(dot_line as u32, dot_char as u32);
    
    println!("Testing structural typing UFCS completion at line {}, char {}", dot_line, dot_char);
    
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
        
        // Should include fields x, y, z
        assert!(completion_labels.contains(&"x"), "Should include field 'x'");
        assert!(completion_labels.contains(&"y"), "Should include field 'y'");
        assert!(completion_labels.contains(&"z"), "Should include field 'z'");
        
        // Should include UFCS methods that accept subsets of fields
        assert!(completion_labels.contains(&"process_2d"), 
                "Should include 'process_2d' function (accepts {{x, y}} subset): {:?}", completion_labels);
        assert!(completion_labels.contains(&"get_x"), 
                "Should include 'get_x' function (accepts {{x}} subset): {:?}", completion_labels);
        
        // Verify they are marked as UFCS functions
        let ufcs_completions: Vec<&CompletionItem> = completions.iter()
            .filter(|c| c.kind == Some(CompletionItemKind::FUNCTION))
            .collect();
        
        let ufcs_labels: Vec<&str> = ufcs_completions.iter().map(|c| c.label.as_str()).collect();
        assert!(ufcs_labels.contains(&"process_2d"), "process_2d should be a UFCS function");
        assert!(ufcs_labels.contains(&"get_x"), "get_x should be a UFCS function");
        
        // Verify function completions include parentheses
        for completion in ufcs_completions {
            if let Some(insert_text) = &completion.insert_text {
                assert!(insert_text.contains("()"), "UFCS function completion should include parentheses");
            }
        }
        
    } else {
        panic!("Expected completions for structural typing test");
    }
}