# WTF Language Server Architecture

This document provides a comprehensive overview of the WTF Language Server Protocol (LSP) implementation architecture, designed for both human developers and Large Language Models (LLMs) who need to understand and work with this codebase.

## Overview

The WTF Language Server is a modular LSP implementation that provides intelligent language support for the WTF programming language. It follows the Language Server Protocol specification and is built using the `tower-lsp` framework.

## Core Components

### 1. Main Entry Point (`main.rs`)
- **Purpose**: Binary entry point that starts the LSP server
- **Functionality**: Sets up stdin/stdout communication and initializes the language server service
- **Architecture**: Simple binary that delegates to the main Backend implementation

### 2. Backend Core (`lib.rs`)
- **Purpose**: Central coordinator and shared state management
- **Key Structures**:
  - `Backend`: Main struct implementing the `LanguageServer` trait
  - Document cache: `HashMap<Url, String>` for storing opened documents
  - AST cache: `HashMap<Url, wtf_ast::Module>` for parsed syntax trees
  - HIR cache: `HashMap<Url, wtf_hir::Module>` for type-checked intermediate representation
- **Responsibilities**:
  - LSP protocol message handling
  - Document lifecycle management (open/change/close)
  - Shared utility methods for AST manipulation
  - Cache coordination between modules

### 3. Diagnostics Module (`diagnostics.rs`)
- **Purpose**: Error detection and reporting
- **Features**:
  - Syntax error detection via `wtf-parser`
  - Semantic error detection via `wtf-hir` compilation
  - Position mapping from byte offsets to LSP positions
  - Error severity classification (ERROR level for proper IDE integration)
- **Error Types**:
  - Parser errors: Malformed syntax, unexpected tokens
  - HIR compilation errors: Type errors, semantic violations
- **Architecture**: Pure functions that transform WTF errors into LSP `Diagnostic` objects

### 4. Completion Module (`completion.rs`)
- **Purpose**: Auto-completion and IntelliSense features
- **Features**:
  - **Field completion**: Suggests record/resource fields after dot notation
  - **Method completion**: UFCS (Uniform Function Call Syntax) support
  - **Identifier completion**: Functions, types, and variables in scope
  - **Keyword completion**: Top-level language keywords with snippets
  - **Structural typing**: Methods available based on field compatibility
- **Key Algorithms**:
  - Context detection: Determines completion type based on cursor position
  - Type inference integration: Uses HIR for accurate type information
  - UFCS resolution: Finds functions callable as methods on given types
  - Structural compatibility: Matches record types based on field subsets

### 5. Code Actions Module (`code_actions.rs`)
- **Purpose**: Quick fixes and refactoring suggestions
- **Features**:
  - **Unknown field corrections**: Suggests valid alternatives for typos
  - **Cross-language keyword mapping**: Converts keywords from other languages
  - **UFCS transformations**: Converts between method and function call syntax
  - **Variable mutability changes**: Toggle between `let` and `var`
  - **Assignment fixes**: Handles assignments to immutable variables
- **Pattern Matching**: Uses regex and AST analysis to detect applicable code actions
- **Text Edits**: Generates precise LSP `TextEdit` objects for IDE integration

### 6. Type Inference Module (`type_inference.rs`)
- **Purpose**: Accurate type resolution using HIR information
- **Features**:
  - **HIR-based inference**: Uses type checker results for accuracy
  - **Variable name mapping**: Links HIR variables back to source names
  - **Anonymous type handling**: Supports record literals without declared types
  - **Fallback mechanisms**: AST-based inference when HIR unavailable
- **Architecture**: Hierarchical type resolution with caching for performance

## Data Flow

### Document Processing Pipeline
1. **Input**: LSP `textDocument/didOpen` or `textDocument/didChange`
2. **Parsing**: `wtf-parser` creates AST, captures syntax errors
3. **Semantic Analysis**: `wtf-hir` performs type checking, captures semantic errors
4. **Caching**: Both AST and HIR stored for subsequent operations
5. **Diagnostics**: Errors converted to LSP format and published
6. **Ready**: Document available for completion and code actions

### Completion Request Flow
1. **Position Analysis**: Determine context (dot notation, identifier, etc.)
2. **Type Inference**: Use HIR to determine expression type at cursor
3. **Candidate Generation**: Based on context, generate completion items
4. **Filtering**: Apply structural typing and UFCS rules
5. **Response**: Return formatted completion items with metadata

### Code Action Request Flow
1. **Pattern Detection**: Analyze text and diagnostics for applicable actions
2. **Action Generation**: Create code actions based on detected patterns
3. **Text Edit Calculation**: Compute precise ranges and replacement text
4. **Response**: Return LSP code actions with workspace edits

## Language Features Supported

### WTF Language Concepts
- **Records**: Structural types with named fields
- **Resources**: Objects with methods and lifecycle management
- **UFCS**: Functions callable as methods when first parameter matches
- **Structural Typing**: Type compatibility based on field presence
- **Anonymous Records**: Record literals without explicit type declarations

### LSP Protocol Compliance
- **Document Synchronization**: Full document sync with change tracking
- **Diagnostics**: Real-time error reporting with severity levels
- **Completion**: Context-aware suggestions with rich metadata
- **Code Actions**: Quick fixes and refactoring capabilities
- **Position Mapping**: Accurate byte-offset to line/character conversion

## Error Handling

### Graceful Degradation
- **Parser Errors**: Continue processing with partial AST
- **HIR Errors**: Fall back to AST-based type inference
- **Cache Misses**: Regenerate from source when needed
- **Invalid Positions**: Handle out-of-bounds requests gracefully

### Error Recovery
- **Incremental Parsing**: Only reparse changed documents
- **Cache Invalidation**: Clear stale data on document changes
- **Partial Results**: Provide best-effort results even with errors

## Performance Considerations

### Caching Strategy
- **Document Cache**: Avoid redundant parsing
- **AST Cache**: Reuse syntax trees for multiple operations
- **HIR Cache**: Expensive type checking cached per document
- **Invalidation**: Cache cleared on document modification

### Lazy Evaluation
- **On-Demand Processing**: HIR compilation only when needed
- **Incremental Updates**: Only recompute affected documents
- **Background Processing**: Future-ready for async compilation

## Extension Points

### Adding New Completion Types
1. Extend `provide_completion` in `completion.rs`
2. Add context detection logic
3. Implement candidate generation
4. Add tests in `completion_test.rs`

### Adding New Code Actions
1. Extend `provide_code_actions` in `code_actions.rs`
2. Add pattern detection logic
3. Implement text edit generation
4. Add integration tests

### Adding New Diagnostics
1. Extend error handling in `diagnostics.rs`
2. Map new error types to LSP diagnostics
3. Ensure proper severity levels
4. Test error scenarios

## Testing Strategy

### Unit Tests
- **Module Tests**: Each module has focused unit tests
- **Integration Tests**: End-to-end LSP functionality
- **Error Scenarios**: Comprehensive error handling coverage
- **Edge Cases**: Boundary conditions and malformed input

### Test Structure
- **`completion_test.rs`**: Comprehensive integration tests
- **Module Tests**: Unit tests within each source module
- **Property Testing**: Validates invariants across inputs

## Dependencies

### Core Dependencies
- **`tower-lsp`**: LSP protocol implementation framework
- **`wtf-parser`**: WTF language parser for AST generation
- **`wtf-hir`**: High-level IR for semantic analysis
- **`wtf-ast`**: Abstract syntax tree definitions
- **`wtf-error`**: Error types and formatting

### Development Dependencies
- **`tokio`**: Async runtime for LSP operations
- **`tower-lsp-types`**: LSP type definitions

## Future Enhancements

### Planned Features
- **Incremental Parsing**: Faster updates for large documents
- **Symbol Search**: Workspace-wide symbol navigation
- **Rename Refactoring**: Safe identifier renaming
- **Go to Definition**: Navigate to symbol definitions
- **Find References**: Locate all symbol usages

### Performance Improvements
- **Background Compilation**: Async HIR processing
- **Partial Recompilation**: Only recompile affected modules
- **Memory Optimization**: Reduce cache memory footprint

## For LLM Assistants

When working with this codebase:

1. **Entry Points**: Start with `lib.rs` for overall structure, then examine specific modules
2. **Test Files**: `completion_test.rs` contains comprehensive examples of expected behavior
3. **Error Handling**: Follow existing patterns in `diagnostics.rs` for consistent error reporting
4. **Type Safety**: Leverage HIR when possible for accurate type information
5. **LSP Compliance**: Ensure all changes maintain LSP protocol compatibility
6. **Performance**: Consider caching and incremental updates for user experience

### Common Patterns
- **Async Methods**: All LSP handlers are async and use `await`
- **Error Propagation**: Use `Option` and `Result` for graceful failure handling
- **Cache Management**: Always update caches when documents change
- **Position Handling**: Convert between byte offsets and LSP positions carefully
- **Text Edits**: Calculate precise ranges for reliable IDE integration

This architecture enables a robust, extensible language server that provides a rich development experience for WTF programmers while maintaining good performance and LSP compliance.