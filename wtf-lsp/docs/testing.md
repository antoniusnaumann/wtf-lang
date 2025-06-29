# Testing Guide

This document provides comprehensive testing guidelines for the WTF Language Server, covering all testing strategies, test organization, and best practices for ensuring robust functionality.

## Test Organization

### Test Structure
```
wtf-lsp/
├── src/
│   ├── lib.rs           # Unit tests for shared utilities
│   ├── completion.rs    # Unit tests for completion logic
│   ├── diagnostics.rs   # Unit tests for error processing
│   ├── code_actions.rs  # Unit tests for action generation
│   └── type_inference.rs # Unit tests for type resolution
└── tests/
    └── completion_test.rs # Integration tests for all features
```

### Test Categories

#### 1. Unit Tests
**Location**: Within source modules using `#[cfg(test)]`
**Purpose**: Test individual functions and algorithms
**Scope**: Single function or small component

#### 2. Integration Tests
**Location**: `tests/` directory
**Purpose**: Test complete LSP workflows
**Scope**: End-to-end functionality with real LSP messages

#### 3. Property Tests
**Purpose**: Validate invariants across input ranges
**Scope**: Edge cases and boundary conditions

## Core Test Patterns

### LSP Service Setup
```rust
use tower_lsp::{LspService, LanguageServer};
use wtf_lsp::Backend;

async fn setup_lsp_service() -> Backend {
    let (service, _socket) = LspService::new(|client| Backend::new(client));
    service.inner()
}
```

### Document Preparation
```rust
async fn prepare_document(backend: &Backend, uri: Url, code: &str) {
    let did_open_params = DidOpenTextDocumentParams {
        text_document: TextDocumentItem {
            uri: uri.clone(),
            language_id: "wtf".to_string(),
            version: 1,
            text: code.to_string(),
        },
    };
    backend.did_open(did_open_params).await;
}
```

### Position Calculation
```rust
fn find_cursor_position(text: &str, marker: &str) -> Position {
    let lines: Vec<&str> = text.lines().collect();
    let line_idx = lines.iter().position(|line| line.contains(marker)).unwrap();
    let char_idx = lines[line_idx].find(marker).unwrap() + marker.len();
    Position::new(line_idx as u32, char_idx as u32)
}
```

## Feature-Specific Testing

### Completion Testing

#### Field Completion Tests
```rust
#[tokio::test]
async fn test_field_completion() {
    let code = r#"
        record Point { x: f64, y: f64 }
        func main() {
            let p = { x: 1.0, y: 2.0 }
            let val = p.● // Test completion here
        }
    "#;
    
    // Setup, prepare document, test completion
    let completions = get_completions_at_marker(code, "p.").await;
    assert_contains_completions(&completions, &["x", "y"]);
}
```

#### Method Completion Tests
```rust
#[tokio::test]
async fn test_ufcs_method_completion() {
    let code = r#"
        record Point { x: f64, y: f64 }
        func distance(p1: Point, p2: Point) -> f64 { 0.0 }
        
        func main() {
            let p = { x: 1.0, y: 2.0 }
            let result = p.● // Should suggest distance method
        }
    "#;
    
    let completions = get_completions_at_marker(code, "p.").await;
    assert_contains_function_completion(&completions, "distance");
}
```

#### Structural Typing Tests
```rust
#[tokio::test]
async fn test_structural_typing_completion() {
    let code = r#"
        record Point2D { x: f64, y: f64 }
        func process_2d(p: Point2D) -> f64 { 0.0 }
        
        func main() {
            let point3d = { x: 1.0, y: 2.0, z: 3.0 }
            let result = point3d.● // Should suggest process_2d
        }
    "#;
    
    let completions = get_completions_at_marker(code, "point3d.").await;
    assert_contains_function_completion(&completions, "process_2d");
}
```

### Code Actions Testing

#### Unknown Field Actions
```rust
#[tokio::test]
async fn test_unknown_field_actions() {
    let code = r#"
        record Point { x: f64, y: f64 }
        func main() {
            let p = { x: 1.0, y: 2.0 }
            let val = p.unknown_field
        }
    "#;
    
    let actions = get_code_actions_for_range(code, "unknown_field").await;
    assert_contains_action_with_title(&actions, "Change 'unknown_field' to 'x'");
    assert_contains_action_with_title(&actions, "Change 'unknown_field' to 'y'");
}
```

#### UFCS Transformation Actions
```rust
#[tokio::test]
async fn test_ufcs_transformation() {
    let code = r#"
        func distance(p1: Point, p2: Point) -> f64 { 0.0 }
        func main() {
            let result = p1.distance(p2) // Test method→function
        }
    "#;
    
    let actions = get_code_actions_for_range(code, "p1.distance(p2)").await;
    assert_contains_action_with_title(&actions, "Convert to function call");
    
    // Verify the text edit is correct
    let action = find_action_by_title(&actions, "Convert to function call").unwrap();
    let edit = extract_text_edit(action).unwrap();
    assert_eq!(edit.new_text, "distance(p1, p2)");
}
```

#### Variable Mutability Actions
```rust
#[tokio::test]
async fn test_mutability_actions() {
    let code = r#"
        func main() {
            let x = 5    // Test let→var
            var y = 10   // Test var→let
        }
    "#;
    
    let let_actions = get_code_actions_for_range(code, "let").await;
    assert_contains_action_with_title(&let_actions, "Change 'let' to 'var'");
    
    let var_actions = get_code_actions_for_range(code, "var").await;
    assert_contains_action_with_title(&var_actions, "Change 'var' to 'let'");
}
```

### Diagnostics Testing

#### Syntax Error Detection
```rust
#[tokio::test]
async fn test_syntax_error_diagnostics() {
    let code = r#"
        func broken() {
            let y = "hello"
            invalid_token$  // Syntax error
        }
    "#;
    
    let diagnostics = validate_document(code).await;
    assert!(!diagnostics.is_empty());
    assert_eq!(diagnostics[0].severity, Some(DiagnosticSeverity::ERROR));
    assert_eq!(diagnostics[0].source, Some("wtf-lsp".to_string()));
}
```

#### Semantic Error Detection
```rust
#[tokio::test]
async fn test_semantic_error_diagnostics() {
    let code = r#"
        func main() {
            let x: string = 42  // Type mismatch
        }
    "#;
    
    let diagnostics = validate_document(code).await;
    assert_contains_error_about(&diagnostics, "type mismatch");
}
```

### Type Inference Testing

#### HIR-Based Inference
```rust
#[tokio::test]
async fn test_hir_type_inference() {
    let code = r#"
        record Point { x: f64, y: f64 }
        func main() {
            let p = Point { x: 1.0, y: 2.0 }
        }
    "#;
    
    let backend = setup_lsp_service().await;
    prepare_document(&backend, test_uri(), code).await;
    
    let inferred_type = backend.infer_expression_type_hir("p", &test_uri()).await;
    assert_eq!(inferred_type, Some("Point".to_string()));
}
```

#### Anonymous Type Inference
```rust
#[tokio::test]
async fn test_anonymous_type_inference() {
    let code = r#"
        func main() {
            let point = { x: 1.0, y: 2.0, z: 3.0 }
        }
    "#;
    
    let inferred_type = infer_type_for_variable("point", code).await;
    assert!(inferred_type.unwrap().starts_with("AnonymousRecord:"));
    assert!(inferred_type.unwrap().contains("x"));
    assert!(inferred_type.unwrap().contains("y"));
    assert!(inferred_type.unwrap().contains("z"));
}
```

## Test Utilities

### Helper Functions
```rust
// Common test utilities
async fn get_completions_at_marker(code: &str, marker: &str) -> Vec<CompletionItem> {
    let backend = setup_lsp_service().await;
    let uri = test_uri();
    prepare_document(&backend, uri.clone(), code).await;
    
    let position = find_cursor_position(code, marker);
    let params = CompletionParams {
        text_document_position: TextDocumentPositionParams {
            text_document: TextDocumentIdentifier { uri },
            position,
        },
        work_done_progress_params: WorkDoneProgressParams::default(),
        partial_result_params: PartialResultParams::default(),
        context: None,
    };
    
    match backend.completion(params).await.unwrap() {
        Some(CompletionResponse::Array(items)) => items,
        _ => vec![],
    }
}

async fn get_code_actions_for_range(code: &str, target: &str) -> Vec<CodeActionOrCommand> {
    let backend = setup_lsp_service().await;
    let uri = test_uri();
    prepare_document(&backend, uri.clone(), code).await;
    
    let range = find_target_range(code, target);
    let diagnostics = backend.validate_document(&uri, code).await;
    
    let params = CodeActionParams {
        text_document: TextDocumentIdentifier { uri },
        range,
        context: CodeActionContext {
            diagnostics,
            only: None,
            trigger_kind: None,
        },
        work_done_progress_params: WorkDoneProgressParams::default(),
        partial_result_params: PartialResultParams::default(),
    };
    
    backend.code_action(params).await.unwrap().unwrap_or_default()
}

fn assert_contains_completions(completions: &[CompletionItem], expected: &[&str]) {
    let labels: Vec<&str> = completions.iter().map(|c| c.label.as_str()).collect();
    for expected_item in expected {
        assert!(labels.contains(expected_item), 
                "Expected completion '{}' not found in {:?}", expected_item, labels);
    }
}

fn assert_contains_function_completion(completions: &[CompletionItem], function_name: &str) {
    let function_completions: Vec<&CompletionItem> = completions.iter()
        .filter(|c| c.kind == Some(CompletionItemKind::FUNCTION))
        .collect();
    
    let found = function_completions.iter()
        .any(|c| c.label == function_name);
    
    assert!(found, "Function '{}' not found in completions", function_name);
    
    // Verify parentheses are included
    let completion = function_completions.iter()
        .find(|c| c.label == function_name)
        .unwrap();
    
    if let Some(insert_text) = &completion.insert_text {
        assert!(insert_text.contains("()"), 
                "Function completion should include parentheses");
    }
}
```

### Assertion Helpers
```rust
fn assert_contains_action_with_title(actions: &[CodeActionOrCommand], title: &str) {
    let found = actions.iter().any(|action| {
        match action {
            CodeActionOrCommand::CodeAction(ca) => ca.title.contains(title),
            _ => false,
        }
    });
    assert!(found, "Action with title '{}' not found", title);
}

fn assert_contains_error_about(diagnostics: &[Diagnostic], message_part: &str) {
    let found = diagnostics.iter()
        .any(|d| d.message.to_lowercase().contains(&message_part.to_lowercase()));
    assert!(found, "No diagnostic found containing '{}'", message_part);
}
```

## Edge Case Testing

### Malformed Input
```rust
#[tokio::test]
async fn test_malformed_syntax() {
    let code = r#"
        func broken( {
            let x = 
        }
    "#;
    
    // Should not crash, should provide diagnostics
    let diagnostics = validate_document(code).await;
    assert!(!diagnostics.is_empty());
    
    // Should still attempt completions
    let completions = get_completions_at_marker(code, "let x = ").await;
    // Completions may be empty, but should not crash
}
```

### Boundary Conditions
```rust
#[tokio::test]
async fn test_empty_document() {
    let completions = get_completions_at_marker("", "").await;
    // Should handle gracefully
}

#[tokio::test]
async fn test_position_at_end_of_file() {
    let code = "func main() {}";
    let position = Position::new(0, code.len() as u32);
    // Should handle out-of-bounds position gracefully
}
```

### Unicode Handling
```rust
#[tokio::test]
async fn test_unicode_identifiers() {
    let code = r#"
        record Point { 坐标x: f64, 坐标y: f64 }
        func main() {
            let p = { 坐标x: 1.0, 坐标y: 2.0 }
            let val = p.●
        }
    "#;
    
    let completions = get_completions_at_marker(code, "p.").await;
    assert_contains_completions(&completions, &["坐标x", "坐标y"]);
}
```

## Performance Testing

### Large Documents
```rust
#[tokio::test]
async fn test_large_document_performance() {
    let mut code = String::new();
    for i in 0..1000 {
        code.push_str(&format!("func func{i}() {{}}\n"));
    }
    code.push_str("func main() { let x = func0; }");
    
    let start = std::time::Instant::now();
    let completions = get_completions_at_marker(&code, "func0").await;
    let duration = start.elapsed();
    
    assert!(!completions.is_empty());
    assert!(duration.as_millis() < 1000, "Completion too slow: {}ms", duration.as_millis());
}
```

### Memory Usage
```rust
#[tokio::test]
async fn test_memory_usage() {
    // Test that caches don't grow unbounded
    let backend = setup_lsp_service().await;
    
    for i in 0..100 {
        let uri = Url::parse(&format!("file:///test{}.wtf", i)).unwrap();
        let code = format!("func test{}() {{}}", i);
        prepare_document(&backend, uri, &code).await;
    }
    
    // Verify reasonable cache sizes
    let ast_cache = backend.ast_cache.read().await;
    let hir_cache = backend.hir_cache.read().await;
    
    assert!(ast_cache.len() <= 100);
    assert!(hir_cache.len() <= 100);
}
```

## Continuous Integration

### Test Commands
```bash
# Run all tests
cargo test -p wtf-lsp

# Run with coverage
cargo test -p wtf-lsp --no-fail-fast -- --test-threads=1

# Run specific test category
cargo test -p wtf-lsp completion
cargo test -p wtf-lsp code_action
cargo test -p wtf-lsp diagnostics
```

### Test Organization
```rust
// Use descriptive test names
#[tokio::test]
async fn test_field_completion_on_declared_record_type() { ... }

#[tokio::test]
async fn test_field_completion_on_anonymous_record_literal() { ... }

#[tokio::test]
async fn test_ufcs_method_completion_with_structural_typing() { ... }
```

## Best Practices

### Test Independence
- Each test should be independent and not rely on state from other tests
- Use fresh LSP service instances for each test
- Clean up resources appropriately

### Clear Assertions
- Use descriptive assertion messages
- Test both positive and negative cases
- Verify edge cases and error conditions

### Realistic Test Data
- Use representative WTF code examples
- Test with various identifier names and types
- Include both simple and complex scenarios

### Performance Awareness
- Include performance regression tests
- Monitor memory usage patterns
- Test with large documents

The comprehensive test suite ensures the WTF Language Server provides reliable, high-quality language support across all use cases and edge conditions.