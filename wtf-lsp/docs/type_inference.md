# Type Inference Module Documentation

The type inference module (`type_inference.rs`) provides accurate type resolution for completion and code action features. This document details the HIR-based type system integration and fallback mechanisms.

## Overview

Type inference bridges the gap between syntax analysis and semantic understanding, enabling intelligent features like field completion and method suggestions. The module leverages the WTF compiler's HIR (High-level Intermediate Representation) for accuracy while providing AST-based fallbacks.

## Architecture

### Two-Tier Inference System

#### 1. HIR-Based Inference (Primary)
**Advantages**: 
- Accurate type checking results
- Handles complex type relationships
- Respects scope and variable shadowing
- Includes type inference results

**Limitations**:
- Requires successful compilation
- Not available for documents with severe syntax errors
- More expensive to compute

#### 2. AST-Based Inference (Fallback)
**Advantages**:
- Works with partial/broken syntax
- Fast and lightweight
- Always available when parsing succeeds
- Good for simple pattern matching

**Limitations**:
- No semantic analysis
- Limited type relationship understanding
- May be inaccurate for complex cases

## HIR Integration

### Variable Name Preservation
The HIR normally doesn't preserve source variable names, but the LSP needs them for completion:

```rust
// Modified HIR to retain variable names
struct FunctionBody {
    var_names: Vec<Option<String>>, // Added for LSP
    // ... other HIR fields
}
```

**Implementation**:
- **Variable Tracking**: `VarCollector` maps HIR indices to source names
- **Named Variables**: Parameters and declarations use `push_named(name)`
- **Anonymous Variables**: Compiler temporaries use `push_anonymous()`
- **Lookup**: LSP searches `var_names` for matches

### Type Mapping
HIR types need to be mapped back to source type names:

```rust
pub async fn infer_expression_type_hir(&self, var_name: &str, uri: &Url) -> Option<String>
```

**Process**:
1. **Cache Access**: Retrieve HIR from document cache
2. **Variable Search**: Find variable in HIR function bodies
3. **Type Extraction**: Get HIR type for variable
4. **Name Mapping**: Convert HIR type back to source name
5. **Anonymous Handling**: Parse synthetic type names

### HIR Type Conversion
```rust
fn hir_type_to_string(&self, hir_type: &wtf_hir::Type) -> String
```

**Type Mapping Rules**:
- **Records**: `Type::Record { name, .. }` → `name`
- **Resources**: `Type::Resource { name, .. }` → `name`
- **Anonymous Records**: Special synthetic names
- **Built-in Types**: Direct string representation

## Anonymous Type Handling

### Synthetic Type Names
For record literals without declared types:

```wtf
let point = { x: 1.0, y: 2.0, z: 3.0 } // No explicit type
```

**HIR Representation**:
- Type name: `"AnonymousRecord:x,y,z"`
- Field list: Encoded in type name for LSP extraction
- Consistent naming: Same fields always generate same name

### Field Extraction
```rust
// Parse anonymous type names to extract fields
if type_name.starts_with("AnonymousRecord:") {
    if let Some(fields_part) = type_name.strip_prefix("AnonymousRecord:") {
        return fields_part.split(',').map(|s| s.to_string()).collect();
    }
}
```

## AST-Based Fallback

### Pattern Matching
When HIR is unavailable, use AST patterns:

```rust
fn infer_expression_type_ast(&self, expression: &str, ast: &wtf_ast::Module) -> Option<String>
```

**Strategies**:
- **Record Literal Matching**: Parse `{ field: value, ... }` patterns
- **Field Count Matching**: Match record types by field count
- **Exact Matching**: Prefer types with identical field sets
- **Best Guess**: Use most similar type when exact match unavailable

### Record Literal Analysis
```rust
// Extract fields from record literal syntax
if expression.trim().starts_with('{') && expression.trim().ends_with('}') {
    let inner = &expression.trim()[1..expression.len()-1];
    // Parse field: value pairs
    // Match against declared record types
}
```

**Field Parsing**:
- **Syntax Recognition**: Identify record literal patterns
- **Field Extraction**: Parse `field: value` pairs
- **Type Matching**: Find records with matching fields
- **Disambiguation**: Prefer exact matches over supersets

## Type Compatibility

### Structural Typing Support
WTF supports structural typing where records with supersets of fields are compatible:

```rust
fn type_is_structurally_compatible(&self, source_type: &str, target_type: &str, ast: &wtf_ast::Module) -> bool
```

**Compatibility Rules**:
- **Subset Matching**: Source must have all fields required by target
- **Field Type Matching**: Field types must be compatible (future enhancement)
- **Anonymous Compatibility**: Anonymous records compatible with any subset

### UFCS Compatibility
For method completion, determine if functions can be called as methods:

```rust
// Function: func process(p: Point2D) -> f64
// Record: { x: f64, y: f64, z: f64 }
// Compatible: record has all required fields (x, y)
```

## Performance Optimizations

### Caching Strategy
```rust
// HIR cache avoids expensive recompilation
hir_cache: tokio::sync::RwLock<HashMap<Url, wtf_hir::Module>>

// AST cache for fallback inference
ast_cache: tokio::sync::RwLock<HashMap<Url, wtf_ast::Module>>
```

### Lazy Evaluation
- **On-Demand HIR**: Only compile HIR when type inference needed
- **Fallback Logic**: Try HIR first, then AST
- **Early Return**: Stop at first successful inference

### Incremental Updates
- **Cache Invalidation**: Clear caches when documents change
- **Selective Recompilation**: Only recompile affected documents
- **Background Processing**: Future enhancement for async compilation

## Error Handling

### Graceful Degradation
```rust
// Always try to provide some result
pub async fn infer_expression_type_hir(&self, var_name: &str, uri: &Url) -> Option<String> {
    // Try HIR first
    if let Some(result) = self.try_hir_inference(var_name, uri).await {
        return Some(result);
    }
    
    // Fall back to AST
    self.try_ast_inference(var_name, uri).await
}
```

### Recovery Strategies
- **Partial HIR**: Use HIR even if compilation has warnings
- **Syntax Errors**: Fall back to AST when HIR compilation fails
- **Cache Misses**: Regenerate from source when needed
- **Invalid Names**: Handle malformed variable names gracefully

## Integration Points

### Completion Module
```rust
// Field completion uses type inference
let type_name = self.infer_expression_type_hir(&var_name, uri).await?;
let fields = self.get_type_fields(&ast, &type_name);
```

### Code Actions Module
```rust
// Unknown field suggestions use type inference
let inferred_type = self.infer_expression_type_hir(&var_name, uri).await?;
let valid_fields = self.get_type_fields(&ast, &inferred_type);
```

### UFCS Discovery
```rust
// Method completion uses structural compatibility
let source_type = self.infer_expression_type_hir(&expression, uri).await?;
let compatible_functions = self.find_structurally_compatible_functions(&source_type, &ast);
```

## Testing Strategy

### HIR Integration Tests
```rust
#[tokio::test]
async fn test_hir_type_inference() {
    // Test variable name preservation
    // Test type mapping accuracy
    // Test anonymous type handling
}
```

### Structural Typing Tests
```rust
#[test]
fn test_structural_compatibility() {
    // Test subset matching
    // Test exact matching
    // Test incompatible types
}
```

### Fallback Tests
```rust
#[tokio::test]
async fn test_ast_fallback() {
    // Test with syntax errors
    // Test with compilation errors
    // Test graceful degradation
}
```

## Future Enhancements

### Enhanced Type System
- **Generic Types**: Support for parameterized types
- **Union Types**: Handle variant types properly
- **Function Types**: First-class function type support
- **Type Aliases**: Resolve type aliases correctly

### Performance Improvements
- **Incremental HIR**: Only recompile changed functions
- **Background Compilation**: Async HIR processing
- **Memory Optimization**: Reduce type information memory usage
- **Caching Refinement**: More granular cache invalidation

### Advanced Features
- **Cross-File Types**: Types from imported modules
- **Type Inference**: Infer types from usage patterns
- **Generic Instantiation**: Resolve generic type parameters
- **Flow-Sensitive Typing**: Track type changes through control flow

## For LLM Assistants

When working with type inference:

1. **HIR Priority**: Always try HIR-based inference first for accuracy
2. **Fallback Strategy**: Implement robust AST fallbacks for error cases
3. **Cache Management**: Update caches when documents change
4. **Anonymous Types**: Handle record literals without explicit types
5. **Structural Typing**: Respect WTF's structural compatibility rules

### Implementation Guidelines

#### Adding New Type Support
```rust
// 1. Add HIR type mapping in hir_type_to_string
// 2. Add AST pattern matching if needed
// 3. Update structural compatibility logic
// 4. Add comprehensive tests
```

#### Debugging Type Inference
```rust
// 1. Check HIR cache availability
// 2. Verify variable name preservation
// 3. Test type mapping accuracy
// 4. Validate structural compatibility
```

### Common Pitfalls
- **Variable Scope**: HIR variables may have different scoping than source
- **Type Aliases**: HIR may resolve aliases that LSP should preserve
- **Anonymous Types**: Synthetic names must be consistent and parseable
- **Cache Consistency**: Ensure HIR and AST caches stay synchronized
- **Unicode Handling**: Variable names may contain non-ASCII characters

The type inference module provides the foundation for accurate language intelligence while maintaining robust fallback behavior for excellent user experience.