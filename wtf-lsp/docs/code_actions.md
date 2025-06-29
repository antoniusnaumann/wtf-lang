# Code Actions Module Documentation

The code actions module (`code_actions.rs`) provides intelligent quick fixes and refactoring capabilities for the WTF Language Server. This document details the implementation, patterns, and algorithms used.

## Overview

Code actions are contextual suggestions that help developers fix errors, refactor code, and improve code quality. The module analyzes code patterns and diagnostics to provide relevant quick fixes.

## Action Categories

### 1. Error Correction Actions

#### Unknown Field Fixes
**Trigger**: Accessing non-existent fields on records/resources
**Context**: `record.unknown_field` → Suggest valid alternatives

```wtf
record Point { x: f64, y: f64 }
let p = Point { x: 1.0, y: 2.0 }
let val = p.unknown // Suggests: Change 'unknown' to 'x' or 'y'
```

**Implementation**:
```rust
fn provide_unknown_field_suggestions(&self, diagnostic: &Diagnostic, text: &str, uri: &Url) -> Vec<CodeAction>
```

**Algorithm**:
1. Parse diagnostic message for field access pattern
2. Extract variable name and attempted field name
3. Infer actual type using HIR
4. Suggest valid field names with edit actions

#### Cross-Language Keyword Mapping
**Trigger**: Using keywords from other programming languages
**Context**: Map common keywords to WTF equivalents

```wtf
def my_function() {     // Quick-fix: Change 'def' to 'func'
    struct Data {       // Quick-fix: Change 'struct' to 'record'
        name: String
    }
}
```

**Mappings**:
- `def`, `fn`, `fun`, `function` → `func`
- `struct` → `record`
- `class` → `resource`, `record`
- `union` → `variant`
- `const` → `let`
- `mut`, `mutable` → `var`
- `elif`, `elseif` → `else if`
- `switch`, `case` → `match`

### 2. Refactoring Actions

#### UFCS Transformations
**Trigger**: Method calls or function calls that can be converted

```wtf
// Method call → Function call
p1.distance(p2) → distance(p1, p2)

// Function call → Method call
distance(p1, p2) → p1.distance(p2)
```

**Implementation**:
```rust
fn create_ufcs_transformation_actions(&self, range: Range, text: &str) -> Vec<CodeAction>
```

**Parsing Logic**:
- **Method Call Pattern**: `receiver.method(args)`
- **Function Call Pattern**: `function(first_arg, rest_args)`
- Smart parentheses handling for nested calls
- Proper argument reordering

#### Variable Mutability Changes
**Trigger**: `let` or `var` keywords in variable declarations

```wtf
let x = 5    // Action: Change 'let' to 'var'
var y = 10   // Action: Change 'var' to 'let'
```

**Implementation**:
```rust
fn create_mutability_change_actions(&self, range: Range, text: &str) -> Vec<CodeAction>
```

### 3. Assignment Error Fixes

#### Immutable Variable Assignment
**Trigger**: Attempting to assign to `let` variables
**Context**: `x = value` where `x` was declared with `let`

```wtf
let x = 5
x = 10  // Two actions:
        // 1. Change to shadowing: let x = 10
        // 2. Make original mutable: var x = 5
```

**Implementation**:
```rust
fn create_assignment_fix_actions(&self, range: Range, text: &str) -> Vec<CodeAction>
```

**Shadowing Action**:
- Replaces assignment with variable declaration
- Preserves indentation from original line
- Calculates exact range for reliable replacement

**Mutability Action**:
- Finds original variable declaration
- Changes `let` to `var` in declaration
- Maintains formatting and spacing

## Text Edit Generation

### Range Calculation
Precise range calculation is critical for reliable IDE integration:

```rust
fn calculate_expression_range(&self, text: &str, line: usize, target: &str) -> Option<Range>
```

**Process**:
1. Parse line content to find target expression
2. Calculate start and end character positions
3. Handle multi-line expressions appropriately
4. Return LSP-compatible Range object

### Text Replacement
```rust
TextEdit {
    range: Range,           // Exact area to replace
    new_text: String,       // Replacement content
}
```

**Strategies**:
- **Token Replacement**: Change specific keywords/identifiers
- **Expression Replacement**: Replace entire expressions
- **Line Replacement**: Replace full lines for assignments
- **Insertion**: Add new content at specific positions

## Pattern Detection

### Regex-Based Detection
For simple patterns like keywords and identifiers:

```rust
const KEYWORD_MAPPINGS: &[(&str, &[&str])] = &[
    ("def", &["func"]),
    ("struct", &["record"]),
    // ...
];
```

### AST-Based Detection
For complex patterns requiring structural analysis:

```rust
fn detect_method_call_pattern(&self, text: &str, range: Range) -> Option<MethodCallInfo>
```

**Benefits**:
- Accurate parsing of complex expressions
- Handles nested parentheses correctly
- Respects language syntax rules
- Provides rich context information

## Error Handling

### Graceful Degradation
```rust
pub async fn provide_code_actions(&self, params: CodeActionParams) -> Option<Vec<CodeActionOrCommand>>
```

**Strategies**:
- Return empty list rather than errors
- Handle malformed text gracefully
- Provide partial results when possible
- Log issues for debugging without failing

### Pattern Matching Robustness
- **Regex Fallbacks**: Multiple patterns for same concept
- **Case Insensitivity**: Handle different casing styles
- **Whitespace Tolerance**: Flexible spacing requirements
- **Syntax Variations**: Support different coding styles

## Performance Considerations

### Lazy Evaluation
- Only analyze ranges when code actions requested
- Cache expensive computations when possible
- Early exit for non-applicable contexts

### Efficient Pattern Matching
- Pre-compiled regex patterns
- Quick context checks before expensive analysis
- Minimal string allocations

## Testing Strategy

### Action Verification Tests
```rust
#[tokio::test]
async fn test_ufcs_transformation_actions() {
    // Test method → function conversion
    // Test function → method conversion
    // Verify text edits are correct
}
```

### Edge Case Coverage
- **Nested Expressions**: Complex call chains
- **Malformed Syntax**: Partial or broken code
- **Boundary Conditions**: Start/end of file
- **Unicode Handling**: Non-ASCII characters

### Integration Tests
- **Full LSP Flow**: Request → Response → Text Edit
- **Multiple Actions**: Ensure no conflicts
- **Range Accuracy**: Verify exact positioning

## Configuration and Customization

### Action Metadata
```rust
CodeAction {
    title: String,                    // Human-readable description
    kind: Option<CodeActionKind>,     // QUICK_FIX, REFACTOR, etc.
    edit: Option<WorkspaceEdit>,      // Text changes to apply
    is_preferred: Option<bool>,       // Highlight in UI
    // ...
}
```

### Action Priorities
- **Error Fixes**: High priority for diagnostic-related actions
- **Refactoring**: Medium priority for code improvements
- **Style Changes**: Lower priority for formatting

## Future Enhancements

### Planned Actions
- **Import Organization**: Sort and group imports
- **Type Inference**: Add missing type annotations
- **Error Recovery**: Suggest fixes for syntax errors
- **Performance**: Identify and fix performance issues

### Advanced Features
- **Multi-Step Refactoring**: Complex transformations requiring multiple edits
- **Cross-File Actions**: Actions affecting multiple documents
- **User Preferences**: Customizable action behavior

## For LLM Assistants

When adding new code actions:

1. **Pattern Identification**: Define clear triggers and contexts
2. **Text Edit Precision**: Calculate exact ranges for reliable replacement
3. **Error Handling**: Graceful failure for edge cases
4. **Testing**: Comprehensive coverage including edge cases
5. **User Experience**: Clear, actionable titles and descriptions

### Implementation Guidelines

#### New Quick Fix Actions
```rust
// 1. Add pattern detection
fn detect_new_pattern(&self, text: &str, range: Range) -> Option<PatternInfo> { ... }

// 2. Add action creation
fn create_new_fix_actions(&self, pattern: PatternInfo) -> Vec<CodeAction> { ... }

// 3. Integrate in main handler
// 4. Add comprehensive tests
```

#### New Refactoring Actions
```rust
// 1. Define transformation rules
// 2. Implement AST analysis if needed
// 3. Create workspace edits
// 4. Test with complex scenarios
```

### Common Pitfalls
- **Range Calculation**: Off-by-one errors in character positions
- **Text Encoding**: Handle UTF-8 properly for character indexing
- **Whitespace Preservation**: Maintain proper indentation
- **Context Sensitivity**: Ensure actions are appropriate for context
- **Performance**: Avoid expensive operations in action detection

The code actions module provides a robust foundation for intelligent code assistance while maintaining excellent performance and user experience.