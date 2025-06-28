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

#[tokio::test]
async fn test_type_inference_with_multiple_types() {
    // Test that type inference correctly distinguishes between multiple record types
    let code = r#"record person {
    name: string
    age: s32
}

record employee {
    name: string
    age: s32
    salary: f64
}

record student {
    name: string
    age: s32
    gpa: f64
}

func demo() {
    // These record literals should be inferred to different types
    let person = { name: "Alice", age: 30 }
    let employee = { name: "Bob", age: 25, salary: 50000.0 }
    let student = { name: "Carol", age: 20, gpa: 3.8 }
    
    // These field access should suggest different fields based on type
    let p_field = person.
    let e_field = employee.
    let s_field = student.
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
    
    // Check AST is available
    let ast_cache = backend.ast_cache.read().await;
    let ast = ast_cache.get(&uri);
    if ast.is_none() {
        panic!("AST should be available but got None - parsing failed");
    }
    
    if let Some(ast) = ast {
        // Verify all record types are found
        let records: Vec<&str> = ast.declarations.iter().filter_map(|decl| {
            match decl {
                wtf_ast::Declaration::Record(r) => Some(r.name.as_str()),
                _ => None,
            }
        }).collect();
        assert!(records.contains(&"person"), "Should find person record");
        assert!(records.contains(&"employee"), "Should find employee record");
        assert!(records.contains(&"student"), "Should find student record");
        
        // Test type inference for each variable
        let person_type = backend.infer_expression_type("person", &uri).await;
        let employee_type = backend.infer_expression_type("employee", &uri).await;
        let student_type = backend.infer_expression_type("student", &uri).await;
        
        println!("Inferred types: person={:?}, employee={:?}, student={:?}", 
                person_type, employee_type, student_type);
        
        // The key test: each should infer to the most specific matching type
        assert_eq!(person_type, Some("person".to_string()), "person should be inferred as person type");
        assert_eq!(employee_type, Some("employee".to_string()), "employee should be inferred as employee type");
        assert_eq!(student_type, Some("student".to_string()), "student should be inferred as student type");
        
        // Test field suggestions for each type
        let person_fields = backend.get_type_fields(ast, "person");
        let employee_fields = backend.get_type_fields(ast, "employee");
        let student_fields = backend.get_type_fields(ast, "student");
        
        // Person should only have name and age
        assert_eq!(person_fields.len(), 2);
        assert!(person_fields.contains(&"name".to_string()));
        assert!(person_fields.contains(&"age".to_string()));
        assert!(!person_fields.contains(&"salary".to_string()));
        assert!(!person_fields.contains(&"gpa".to_string()));
        
        // Employee should have name, age, and salary
        assert_eq!(employee_fields.len(), 3);
        assert!(employee_fields.contains(&"name".to_string()));
        assert!(employee_fields.contains(&"age".to_string()));
        assert!(employee_fields.contains(&"salary".to_string()));
        assert!(!employee_fields.contains(&"gpa".to_string()));
        
        // Student should have name, age, and gpa
        assert_eq!(student_fields.len(), 3);
        assert!(student_fields.contains(&"name".to_string()));
        assert!(student_fields.contains(&"age".to_string()));
        assert!(student_fields.contains(&"gpa".to_string()));
        assert!(!student_fields.contains(&"salary".to_string()));
    }
    drop(ast_cache);
    
    // Test completion for person (should only suggest name, age)
    let lines: Vec<&str> = code.lines().collect();
    let person_line = lines.iter().position(|line| line.contains("let p_field = person.")).unwrap();
    let person_char = lines[person_line].find("person.").unwrap() + 7;
    let person_position = Position::new(person_line as u32, person_char as u32);
    
    let person_completion_params = CompletionParams {
        text_document_position: TextDocumentPositionParams {
            text_document: TextDocumentIdentifier { uri: uri.clone() },
            position: person_position,
        },
        work_done_progress_params: WorkDoneProgressParams::default(),
        partial_result_params: PartialResultParams::default(),
        context: None,
    };
    
    let person_completions = backend.completion(person_completion_params).await.unwrap();
    if let Some(CompletionResponse::Array(completions)) = person_completions {
        let labels: Vec<&str> = completions.iter().map(|c| c.label.as_str()).collect();
        println!("Person completions: {:?}", labels);
        
        // Person should only get name and age, not salary or gpa
        assert!(labels.contains(&"name"), "Person should have name field");
        assert!(labels.contains(&"age"), "Person should have age field");
        assert!(!labels.contains(&"salary"), "Person should NOT have salary field");
        assert!(!labels.contains(&"gpa"), "Person should NOT have gpa field");
    } else {
        panic!("Expected completions for person but got None");
    }
    
    // Test completion for employee (should suggest name, age, salary)
    let employee_line = lines.iter().position(|line| line.contains("let e_field = employee.")).unwrap();
    let employee_char = lines[employee_line].find("employee.").unwrap() + 9;
    let employee_position = Position::new(employee_line as u32, employee_char as u32);
    
    let employee_completion_params = CompletionParams {
        text_document_position: TextDocumentPositionParams {
            text_document: TextDocumentIdentifier { uri: uri.clone() },
            position: employee_position,
        },
        work_done_progress_params: WorkDoneProgressParams::default(),
        partial_result_params: PartialResultParams::default(),
        context: None,
    };
    
    let employee_completions = backend.completion(employee_completion_params).await.unwrap();
    if let Some(CompletionResponse::Array(completions)) = employee_completions {
        let labels: Vec<&str> = completions.iter().map(|c| c.label.as_str()).collect();
        println!("Employee completions: {:?}", labels);
        
        // Employee should get name, age, and salary, but not gpa
        assert!(labels.contains(&"name"), "Employee should have name field");
        assert!(labels.contains(&"age"), "Employee should have age field");
        assert!(labels.contains(&"salary"), "Employee should have salary field");
        assert!(!labels.contains(&"gpa"), "Employee should NOT have gpa field");
    } else {
        panic!("Expected completions for employee but got None");
    }
    
    // Test completion for student (should suggest name, age, gpa)
    let student_line = lines.iter().position(|line| line.contains("let s_field = student.")).unwrap();
    let student_char = lines[student_line].find("student.").unwrap() + 8;
    let student_position = Position::new(student_line as u32, student_char as u32);
    
    let student_completion_params = CompletionParams {
        text_document_position: TextDocumentPositionParams {
            text_document: TextDocumentIdentifier { uri: uri.clone() },
            position: student_position,
        },
        work_done_progress_params: WorkDoneProgressParams::default(),
        partial_result_params: PartialResultParams::default(),
        context: None,
    };
    
    let student_completions = backend.completion(student_completion_params).await.unwrap();
    if let Some(CompletionResponse::Array(completions)) = student_completions {
        let labels: Vec<&str> = completions.iter().map(|c| c.label.as_str()).collect();
        println!("Student completions: {:?}", labels);
        
        // Student should get name, age, and gpa, but not salary
        assert!(labels.contains(&"name"), "Student should have name field");
        assert!(labels.contains(&"age"), "Student should have age field");
        assert!(labels.contains(&"gpa"), "Student should have gpa field");
        assert!(!labels.contains(&"salary"), "Student should NOT have salary field");
    } else {
        panic!("Expected completions for student but got None");
    }
}

#[tokio::test]
async fn test_type_inference_ambiguous_records() {
    // Test case where multiple record types match the same field set (ambiguous)
    let code = r#"record person {
    name: string
    age: s32
}

record user {
    name: string
    age: s32
}

record customer {
    name: string
    age: s32
    email: string
}

func demo() {
    // This record literal could match person OR user (both have only name, age)
    let ambiguous = { name: "Alice", age: 30 }
    
    // This should clearly be customer due to the email field
    let clear = { name: "Bob", age: 25, email: "bob@example.com" }
    
    // Test field completion for ambiguous case
    let a_field = ambiguous.
    let c_field = clear.
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
    
    // Check AST is available
    let ast_cache = backend.ast_cache.read().await;
    let ast = ast_cache.get(&uri);
    if ast.is_none() {
        panic!("AST should be available but got None - parsing failed");
    }
    
    if let Some(ast) = ast {
        // Test type inference - the issue might be here
        let ambiguous_type = backend.infer_expression_type("ambiguous", &uri).await;
        let clear_type = backend.infer_expression_type("clear", &uri).await;
        
        println!("Inferred types: ambiguous={:?}, clear={:?}", ambiguous_type, clear_type);
        
        // The problem: ambiguous record literal { name: "Alice", age: 30 } could be person OR user
        // Current implementation might always pick the first match (person) instead of considering context
        
        // Clear should definitely be customer (has unique email field)
        assert_eq!(clear_type, Some("customer".to_string()), "clear should be inferred as customer type");
        
        // For ambiguous, the current algorithm might pick the first match which could be wrong
        // This test documents the current behavior - it might pick "person" when "user" was intended
        if let Some(inferred) = &ambiguous_type {
            println!("Ambiguous type was inferred as: {}", inferred);
            // With improved algorithm, it should pick one of the exact matches (person or user)
            // Since both have exactly 2 fields, either is valid
            assert!(inferred == "person" || inferred == "user", 
                    "Ambiguous should be inferred as either person or user, got: {}", inferred);
        }
    }
    drop(ast_cache);
    
    // Test completion for ambiguous - this is where the wrong suggestions might appear
    let lines: Vec<&str> = code.lines().collect();
    let ambiguous_line = lines.iter().position(|line| line.contains("let a_field = ambiguous.")).unwrap();
    let ambiguous_char = lines[ambiguous_line].find("ambiguous.").unwrap() + 10;
    let ambiguous_position = Position::new(ambiguous_line as u32, ambiguous_char as u32);
    
    let ambiguous_completion_params = CompletionParams {
        text_document_position: TextDocumentPositionParams {
            text_document: TextDocumentIdentifier { uri: uri.clone() },
            position: ambiguous_position,
        },
        work_done_progress_params: WorkDoneProgressParams::default(),
        partial_result_params: PartialResultParams::default(),
        context: None,
    };
    
    let ambiguous_completions = backend.completion(ambiguous_completion_params).await.unwrap();
    if let Some(CompletionResponse::Array(completions)) = ambiguous_completions {
        let labels: Vec<&str> = completions.iter().map(|c| c.label.as_str()).collect();
        println!("Ambiguous completions: {:?}", labels);
        
        // Should only suggest name and age (common fields), not email
        assert!(labels.contains(&"name"), "Should suggest name field");
        assert!(labels.contains(&"age"), "Should suggest age field");
        assert!(!labels.contains(&"email"), "Should NOT suggest email field for ambiguous type");
        
        // The problem reported might be that it suggests fields from the wrong type
        // For example, if it incorrectly inferred customer instead of person/user
    } else {
        panic!("Expected completions for ambiguous but got None");
    }
    
    // Test completion for clear - should be unambiguous 
    let clear_line = lines.iter().position(|line| line.contains("let c_field = clear.")).unwrap();
    let clear_char = lines[clear_line].find("clear.").unwrap() + 6;
    let clear_position = Position::new(clear_line as u32, clear_char as u32);
    
    let clear_completion_params = CompletionParams {
        text_document_position: TextDocumentPositionParams {
            text_document: TextDocumentIdentifier { uri: uri.clone() },
            position: clear_position,
        },
        work_done_progress_params: WorkDoneProgressParams::default(),
        partial_result_params: PartialResultParams::default(),
        context: None,
    };
    
    let clear_completions = backend.completion(clear_completion_params).await.unwrap();
    if let Some(CompletionResponse::Array(completions)) = clear_completions {
        let labels: Vec<&str> = completions.iter().map(|c| c.label.as_str()).collect();
        println!("Clear completions: {:?}", labels);
        
        // Should suggest all customer fields
        assert!(labels.contains(&"name"), "Customer should have name field");
        assert!(labels.contains(&"age"), "Customer should have age field");
        assert!(labels.contains(&"email"), "Customer should have email field");
    } else {
        panic!("Expected completions for clear but got None");
    }
}

#[tokio::test]
async fn test_improved_type_inference_specificity() {
    // Test that improved type inference picks the most specific match
    let code = r#"record base {
    id: s32
}

record extended {
    id: s32
    name: string
    extra: string
}

record target {
    id: s32
    name: string
}

func demo() {
    // This should match 'target' exactly (2 fields), not 'extended' (3 fields)
    let exact_match = { id: 1, name: "test" }
    
    // This should match 'extended' (all 3 fields)
    let extended_match = { id: 2, name: "test", extra: "data" }
    
    // This should match 'base' exactly (1 field), not others
    let base_match = { id: 3 }
    
    let exact_field = exact_match.
    let extended_field = extended_match.
    let base_field = base_match.
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
    
    // Check AST is available
    let ast_cache = backend.ast_cache.read().await;
    let ast = ast_cache.get(&uri);
    if ast.is_none() {
        panic!("AST should be available but got None - parsing failed");
    }
    
    if let Some(_ast) = ast {
        // Test type inference - should pick the most specific matches
        let exact_type = backend.infer_expression_type("exact_match", &uri).await;
        let extended_type = backend.infer_expression_type("extended_match", &uri).await;
        let base_type = backend.infer_expression_type("base_match", &uri).await;
        
        println!("Inferred types: exact_match={:?}, extended_match={:?}, base_match={:?}", 
                exact_type, extended_type, base_type);
        
        // Key tests for improved type inference:
        
        // exact_match should be inferred as 'target' (exact match with 2 fields)
        // NOT 'extended' (which has 3 fields but only 2 are used)
        assert_eq!(exact_type, Some("target".to_string()), 
                  "exact_match should be inferred as 'target' (exact field match), not 'extended'");
        
        // extended_match should be inferred as 'extended' (exact match with 3 fields)
        assert_eq!(extended_type, Some("extended".to_string()), 
                  "extended_match should be inferred as 'extended'");
        
        // base_match should be inferred as 'base' (exact match with 1 field)
        // NOT 'target' or 'extended' (which have more fields)
        assert_eq!(base_type, Some("base".to_string()), 
                  "base_match should be inferred as 'base' (exact field match), not 'target' or 'extended'");
    }
    drop(ast_cache);
    
    // Test completions to verify correct field suggestions
    let lines: Vec<&str> = code.lines().collect();
    
    // Test exact_match completion (should suggest only id, name)
    let exact_line = lines.iter().position(|line| line.contains("let exact_field = exact_match.")).unwrap();
    let exact_char = lines[exact_line].find("exact_match.").unwrap() + 12;
    let exact_position = Position::new(exact_line as u32, exact_char as u32);
    
    let exact_completion_params = CompletionParams {
        text_document_position: TextDocumentPositionParams {
            text_document: TextDocumentIdentifier { uri: uri.clone() },
            position: exact_position,
        },
        work_done_progress_params: WorkDoneProgressParams::default(),
        partial_result_params: PartialResultParams::default(),
        context: None,
    };
    
    let exact_completions = backend.completion(exact_completion_params).await.unwrap();
    if let Some(CompletionResponse::Array(completions)) = exact_completions {
        let labels: Vec<&str> = completions.iter().map(|c| c.label.as_str()).collect();
        println!("Exact match completions: {:?}", labels);
        
        // Should suggest only target fields (id, name), not extra from extended
        assert!(labels.contains(&"id"), "Should suggest id field");
        assert!(labels.contains(&"name"), "Should suggest name field");
        assert!(!labels.contains(&"extra"), "Should NOT suggest extra field (from wrong type 'extended')");
        assert_eq!(labels.len(), 2, "Should suggest exactly 2 fields for target type");
    } else {
        panic!("Expected completions for exact_match but got None");
    }
    
    // Test base_match completion (should suggest only id)
    let base_line = lines.iter().position(|line| line.contains("let base_field = base_match.")).unwrap();
    let base_char = lines[base_line].find("base_match.").unwrap() + 11;
    let base_position = Position::new(base_line as u32, base_char as u32);
    
    let base_completion_params = CompletionParams {
        text_document_position: TextDocumentPositionParams {
            text_document: TextDocumentIdentifier { uri: uri.clone() },
            position: base_position,
        },
        work_done_progress_params: WorkDoneProgressParams::default(),
        partial_result_params: PartialResultParams::default(),
        context: None,
    };
    
    let base_completions = backend.completion(base_completion_params).await.unwrap();
    if let Some(CompletionResponse::Array(completions)) = base_completions {
        let labels: Vec<&str> = completions.iter().map(|c| c.label.as_str()).collect();
        println!("Base match completions: {:?}", labels);
        
        // Should suggest only base fields (id), not name/extra from other types
        assert!(labels.contains(&"id"), "Should suggest id field");
        assert!(!labels.contains(&"name"), "Should NOT suggest name field (from wrong types)");
        assert!(!labels.contains(&"extra"), "Should NOT suggest extra field (from wrong types)");
        assert_eq!(labels.len(), 1, "Should suggest exactly 1 field for base type");
    } else {
        panic!("Expected completions for base_match but got None");
    }
}