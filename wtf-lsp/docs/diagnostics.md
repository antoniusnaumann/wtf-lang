# Diagnostics Module Documentation

The diagnostics module (`diagnostics.rs`) handles error detection and reporting for the WTF Language Server. This document details how errors are detected, processed, and converted to LSP diagnostics.

## Overview

The diagnostics system provides real-time error reporting by integrating with the WTF compiler pipeline. It processes both syntax errors from the parser and semantic errors from the HIR compiler, presenting them as LSP diagnostics with appropriate severity levels.

## Error Sources

### 1. Parser Errors (Syntax)
**Source**: `wtf-parser` crate
**Types**: Malformed syntax, unexpected tokens, missing constructs

```wtf
func broken() {
    let y = "hello"
    some_invalid_token$  // ← Syntax error
    return y
```

**Common Parser Errors**:
- Unexpected characters or tokens
- Missing closing braces, parentheses
- Invalid identifiers
- Malformed expressions
- Incomplete function definitions

### 2. HIR Compilation Errors (Semantic)
**Source**: `wtf-hir` crate  
**Types**: Type errors, undefined variables, semantic violations

```wtf
func main() {
    let x: string = 42    // Type mismatch
    let y = undefined_var // Undefined variable
}
```

**Common HIR Errors**:
- Type mismatches
- Undefined variables/functions
- Invalid operations
- Scope violations
- Resource lifecycle errors

## Error Processing Pipeline

### 1. Document Validation Entry Point
```rust
pub async fn validate_document(&self, uri: &Url, text: &str) -> Vec<Diagnostic>
```

**Process Flow**:
1. **Parse Document**: Create AST using `wtf-parser`
2. **Collect Parser Errors**: Extract syntax errors from parser
3. **Cache AST**: Store for completion and code actions
4. **Compile HIR**: Attempt semantic analysis
5. **Collect HIR Errors**: Extract semantic errors
6. **Cache HIR**: Store for type inference
7. **Convert to Diagnostics**: Transform errors to LSP format

### 2. Error Conversion
```rust
fn wtf_error_to_diagnostic(&self, error: &WtfError, source_chars: &[char]) -> Diagnostic
```

**Conversion Process**:
- **Position Mapping**: Convert byte offsets to line/character positions
- **Severity Assignment**: Set appropriate diagnostic severity
- **Message Formatting**: Create human-readable error messages
- **Source Attribution**: Tag with "wtf-lsp" for tracking

## Position Mapping

### Byte Offset to LSP Position
WTF errors use byte offsets, but LSP requires line/character positions:

```rust
fn byte_offset_to_position(&self, offset: usize, source_chars: &[char]) -> Position
```

**Algorithm**:
1. Iterate through characters counting lines and columns
2. Handle different line ending styles (LF, CRLF)
3. Track Unicode character boundaries correctly
4. Return LSP `Position` object

**Challenges**:
- **Unicode Handling**: Multi-byte characters require careful counting
- **Line Endings**: Different platforms use different conventions
- **Performance**: Efficient for large documents

### Range Calculation
For errors spanning multiple characters:

```rust
Range::new(start_position, end_position)
```

**Strategies**:
- **Single Character**: Point errors for specific tokens
- **Token Spans**: Highlight entire problematic tokens
- **Expression Ranges**: Cover complete expressions when relevant

## Diagnostic Metadata

### Severity Levels
```rust
DiagnosticSeverity::ERROR  // For both parser and HIR errors
```

**Rationale**:
- **Consistent Experience**: All WTF errors are blocking issues
- **IDE Integration**: Proper error highlighting and reporting
- **Build Integration**: Compatible with CI/CD error reporting

### Diagnostic Structure
```rust
Diagnostic {
    range: Range,                           // Position in document
    severity: Some(DiagnosticSeverity::ERROR), // Error level
    source: Some("wtf-lsp".to_string()),    // Source identifier
    message: String,                        // Human-readable description
    code: None,                            // Future: error codes
    related_information: None,             // Future: additional context
}
```

## Error Handling Strategies

### Graceful Degradation
```rust
// Continue processing even with parser errors
match parse_result {
    Ok(ast) => {
        // Cache successful AST
        // Attempt HIR compilation
    },
    Err(_) => {
        // Still try partial processing
        // Return diagnostics for syntax errors
    }
}
```

**Benefits**:
- **Partial Results**: Provide completions even with syntax errors
- **Incremental Fixing**: Users can fix errors one at a time
- **Better UX**: Language server remains responsive

### Error Recovery
- **Cache Management**: Clear stale data when documents change
- **Incremental Updates**: Only reprocess changed documents
- **Fallback Modes**: Degrade gracefully when compilation fails

## Performance Optimizations

### Caching Strategy
```rust
// Document cache for avoiding redundant parsing
documents: tokio::sync::RwLock<HashMap<Url, String>>,

// AST cache for reusing parsed trees
ast_cache: tokio::sync::RwLock<HashMap<Url, wtf_ast::Module>>,

// HIR cache for reusing compiled representations
hir_cache: tokio::sync::RwLock<HashMap<Url, wtf_hir::Module>>,
```

### Incremental Processing
- **Change Detection**: Only revalidate modified documents
- **Dependency Tracking**: Future enhancement for multi-file projects
- **Background Processing**: Async validation doesn't block UI

## Integration with Language Server

### Document Lifecycle
```rust
// textDocument/didOpen
backend.did_open(params).await;

// textDocument/didChange  
backend.did_change(params).await;

// textDocument/didClose
backend.did_close(params).await;
```

**Validation Triggers**:
- **Document Open**: Initial validation and caching
- **Document Change**: Revalidation and cache update
- **Manual Request**: On-demand validation

### Diagnostic Publishing
```rust
// Automatic publication to LSP client
self.client.publish_diagnostics(uri.clone(), diagnostics, None).await;
```

**Features**:
- **Real-time Updates**: Diagnostics sent as user types
- **Incremental Changes**: Only affected diagnostics updated
- **Clear on Fix**: Diagnostics removed when errors resolved

## Error Message Quality

### User-Friendly Messages
- **Clear Descriptions**: Avoid technical jargon when possible
- **Actionable Suggestions**: Hint at potential fixes
- **Context Information**: Show relevant code snippets

### Example Transformations
```rust
// Parser error: "Expected '}', found 'EOF'"
// Enhanced: "Missing closing brace '}' - function body is not properly closed"

// HIR error: "Type mismatch: expected String, found Int"
// Enhanced: "Cannot assign integer value to string variable - consider using string conversion"
```

## Testing Strategy

### Unit Tests
```rust
#[test]
fn test_position_mapping() {
    // Test byte offset to position conversion
    // Test unicode character handling
    // Test different line ending styles
}

#[test]
fn test_error_conversion() {
    // Test parser error to diagnostic conversion
    // Test HIR error to diagnostic conversion
    // Test severity assignment
}
```

### Integration Tests
- **End-to-End Validation**: Full document processing pipeline
- **Error Scenarios**: Various types of syntax and semantic errors
- **Performance Tests**: Large documents and many errors
- **Edge Cases**: Empty documents, invalid UTF-8, etc.

## Future Enhancements

### Enhanced Error Information
- **Error Codes**: Structured error identification
- **Related Information**: Link related errors together
- **Suggestions**: Automatic fix suggestions in diagnostics
- **Severity Refinement**: More granular severity levels

### Performance Improvements
- **Incremental Parsing**: Only reparse changed portions
- **Background Validation**: Validate while user types
- **Dependency Tracking**: Multi-file error propagation
- **Memory Optimization**: Reduce diagnostic memory footprint

### Advanced Features
- **Error Filtering**: User-configurable error categories
- **Custom Messages**: Project-specific error formatting
- **Batch Processing**: Efficient handling of many errors
- **Error Analytics**: Track common error patterns

## For LLM Assistants

When working with diagnostics:

1. **Error Preservation**: Always preserve original error information
2. **Position Accuracy**: Ensure precise mapping from byte offsets
3. **Message Quality**: Create clear, actionable error messages
4. **Performance**: Consider caching and incremental updates
5. **Testing**: Verify position mapping and error conversion

### Common Implementation Patterns

#### Adding New Error Types
```rust
// 1. Identify error source (parser vs HIR)
// 2. Add conversion logic in wtf_error_to_diagnostic
// 3. Test with representative error cases
// 4. Verify position mapping accuracy
```

#### Improving Error Messages
```rust
// 1. Analyze existing error patterns
// 2. Create user-friendly alternatives
// 3. Maintain technical accuracy
// 4. Test with real user scenarios
```

### Common Pitfalls
- **Position Mapping**: Off-by-one errors in line/character calculation
- **Unicode Handling**: Incorrect character boundary detection
- **Cache Invalidation**: Stale diagnostics after document changes
- **Performance**: Expensive recomputation on every change
- **Error Duplication**: Same error reported multiple times

The diagnostics module provides the foundation for excellent error reporting while maintaining high performance and accurate position tracking.