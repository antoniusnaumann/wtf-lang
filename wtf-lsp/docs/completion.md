# Completion Module Documentation

The completion module (`completion.rs`) provides intelligent auto-completion features for the WTF Language Server. This document details the implementation, algorithms, and usage patterns.

## Overview

The completion system provides context-aware suggestions based on cursor position and surrounding code. It supports multiple completion types with rich metadata for optimal IDE integration.

## Completion Types

### 1. Field Completion (Dot Notation)
**Trigger**: User types `.` after an expression
**Context**: `expression.●` (cursor after dot)

```wtf
record Point { x: f64, y: f64 }
let p = { x: 1.0, y: 2.0 }
p.● // Suggests: x, y
```

**Algorithm**:
1. Parse backward from cursor to find expression before dot
2. Infer type of expression using HIR
3. Extract field names from record/resource type
4. Return field completions with FIELD kind

### 2. Method Completion (UFCS)
**Trigger**: User types `.` after an expression
**Context**: Functions callable as methods via UFCS

```wtf
func distance(p1: Point, p2: Point) -> f64 { ... }
let p1 = { x: 1.0, y: 2.0 }
p1.● // Suggests: distance(p2: Point) -> f64
```

**Algorithm**:
1. Determine expression type at cursor
2. Find all functions where first parameter is compatible
3. Apply structural typing rules for record compatibility
4. Return method completions with FUNCTION kind and parentheses

### 3. Identifier Completion
**Trigger**: User types in identifier context
**Context**: Function names, type names, variable names

```wtf
func main() {
    let p = Point { x: 1.0, y: 2.0 }
    dis● // Suggests: distance()
}
```

**Algorithm**:
1. Detect identifier context (not after dot)
2. Extract available symbols from AST
3. Filter by prefix if partial identifier exists
4. Return with appropriate kinds (FUNCTION, TYPE, VARIABLE)

### 4. Keyword Completion
**Trigger**: User types at top level or statement beginning
**Context**: Language keywords and constructs

```wtf
// Top level
fun● // Suggests: func (with snippet)
rec● // Suggests: record (with snippet)
```

**Algorithm**:
1. Detect top-level or statement context
2. Return keyword completions with KEYWORD kind
3. Include snippets for common patterns

## Type Inference Integration

### HIR-Based Inference
The completion system leverages HIR (High-level Intermediate Representation) for accurate type information:

```rust
async fn infer_expression_type_hir(&self, var_name: &str, uri: &Url) -> Option<String>
```

**Process**:
1. Access cached HIR for document
2. Search variable names in HIR function bodies
3. Map HIR types back to original type names
4. Handle anonymous record types specially

### Fallback to AST
When HIR is unavailable or incomplete:

```rust
fn infer_expression_type_ast(&self, expression: &str, ast: &wtf_ast::Module) -> Option<String>
```

**Process**:
1. Parse record literals for field patterns
2. Match against declared record types
3. Use exact field count matching for disambiguation
4. Generate anonymous type identifiers

## Structural Typing Support

### Record Compatibility
WTF supports structural typing where records with supersets of fields are compatible:

```wtf
record Point2D { x: f64, y: f64 }
func process_2d(p: Point2D) -> f64 { ... }

let point3d = { x: 1.0, y: 2.0, z: 3.0 }
point3d.process_2d() // Valid: point3d has all required fields
```

**Implementation**:
```rust
fn type_is_structurally_compatible(&self, source_type: &str, target_type: &str, ast: &wtf_ast::Module) -> bool
```

### UFCS Method Discovery
Uniform Function Call Syntax allows functions to be called as methods:

```rust
async fn find_ufcs_methods(&self, type_name: &str, uri: &Url) -> Vec<CompletionItem>
```

**Process**:
1. Find all functions in AST
2. Check if first parameter type is compatible with expression type
3. Apply structural typing rules
4. Format as method completions with proper signatures

## Anonymous Type Handling

### Anonymous Records
Records created without explicit type declarations:

```wtf
let point = { x: 1.0, y: 2.0, z: 3.0 } // No declared type
point.● // Should still provide x, y, z completions
```

**Implementation**:
- HIR creates synthetic type names like `AnonymousRecord:x,y,z`
- Completion system parses these identifiers to extract field names
- Provides same completion experience as declared types

## Performance Optimizations

### Caching Strategy
- **AST Cache**: Reuse parsed syntax trees
- **HIR Cache**: Avoid redundant type checking
- **Lazy Evaluation**: Only compute completions when requested

### Incremental Processing
- **Context Detection**: Quick rejection of non-completion contexts
- **Prefix Filtering**: Early filtering reduces candidate set
- **Parallel Processing**: Future-ready for concurrent completion

## Error Handling

### Graceful Degradation
```rust
// Always return Some result, even if empty
pub async fn provide_completion(&self, params: CompletionParams) -> Option<CompletionResponse>
```

**Strategies**:
- Return empty completions rather than errors
- Fall back to AST when HIR unavailable
- Provide partial results when parsing fails
- Handle invalid cursor positions gracefully

### Recovery Mechanisms
- **Parser Errors**: Use partial AST for best-effort completion
- **Type Errors**: Fall back to syntactic completion
- **Cache Misses**: Regenerate from source transparently

## Configuration and Customization

### Completion Item Metadata
```rust
CompletionItem {
    label: String,           // Display name
    kind: CompletionItemKind, // FIELD, FUNCTION, TYPE, etc.
    detail: Option<String>,  // Type signature
    insert_text: Option<String>, // Text to insert (with parentheses for functions)
    // ...
}
```

### Snippet Support
For keywords and common patterns:
```rust
insert_text: Some("func ${1:name}(${2:params}) -> ${3:return_type} {\n    ${4:body}\n}".to_string())
```

## Testing Strategy

### Unit Tests
Each completion type has dedicated tests:
```rust
#[tokio::test]
async fn test_completion_basic_infrastructure() { ... }

#[tokio::test] 
async fn test_anonymous_type_completion() { ... }

#[tokio::test]
async fn test_structural_typing_ufcs_completion() { ... }
```

### Integration Tests
End-to-end scenarios testing real LSP interactions:
- Document setup with `didOpen`
- Cursor positioning
- Completion request/response
- Result validation

### Edge Case Coverage
- Malformed syntax
- Partial identifiers
- Out-of-bounds positions
- Empty documents
- Complex type hierarchies

## Future Enhancements

### Planned Features
- **Fuzzy Matching**: Suggest completions for typos
- **Ranking**: Sort completions by relevance
- **Context Awareness**: Different completions in different contexts
- **Import Suggestions**: Auto-import for external symbols

### Performance Improvements
- **Incremental Completion**: Update completions as user types
- **Background Processing**: Pre-compute common completions
- **Memory Optimization**: Reduce completion item overhead

## For LLM Assistants

When extending completion functionality:

1. **Follow Patterns**: Use existing `get_*_completions` methods as templates
2. **Type Safety**: Always use HIR when available for accuracy
3. **Error Handling**: Return empty rather than failing
4. **Testing**: Add comprehensive tests for new completion types
5. **Performance**: Consider caching and lazy evaluation
6. **LSP Compliance**: Ensure completion items follow LSP specification

### Common Pitfalls
- **Position Mapping**: Convert between byte offsets and line/character carefully
- **Cache Invalidation**: Clear caches when documents change
- **Type Inference**: Handle both declared and anonymous types
- **UFCS Compatibility**: Apply structural typing rules correctly
- **Text Insertion**: Include proper formatting (parentheses, etc.)

The completion module is designed for extensibility while maintaining high performance and accuracy through integration with the WTF compiler pipeline.