# WTF Language Server (wtf-lsp)

A Language Server Protocol (LSP) implementation for the WTF programming language.

## Features

- **Syntax Error Detection**: Real-time parsing and syntax error reporting
- **HIR Error Detection**: High-level intermediate representation compilation errors
- **Document Synchronization**: Support for opening, editing, and closing WTF files
- **Position Mapping**: Accurate mapping of byte offsets to line/character positions
- **Auto-completion**: Method completion with "." trigger supporting UFCS and structural typing
- **Code Actions**: Quick fixes for common programming errors

## Installation

Build the language server:

```bash
cargo build -p wtf-lsp --release
```

The binary will be available at `target/release/wtf-lsp`.

## Usage

The language server communicates via stdin/stdout using the LSP protocol. It can be integrated with any LSP-compatible editor.

### VS Code

To use with VS Code, you would need to create an extension that configures the language server. Add this to your VS Code settings or extension:

```json
{
  "wtf": {
    "command": "/path/to/wtf-lsp",
    "args": [],
    "filetypes": ["wtf"]
  }
}
```

### Neovim

For Neovim with nvim-lspconfig:

```lua
local lspconfig = require('lspconfig')
local configs = require('lspconfig.configs')

-- Configure the WTF language server
if not configs.wtf then
  configs.wtf = {
    default_config = {
      cmd = { '/path/to/wtf-lsp' },
      filetypes = { 'wtf' },
      root_dir = lspconfig.util.root_pattern('.git', 'Cargo.toml'),
      settings = {},
    },
  }
end

lspconfig.wtf.setup{}
```

## Capabilities

The language server currently supports:

- `textDocument/didOpen`
- `textDocument/didChange` 
- `textDocument/didClose`
- `textDocument/publishDiagnostics`
- `textDocument/codeAction` (provides quick fixes for common errors)
- `textDocument/completion` (method auto-completion with "." trigger)

## Error Types

The language server detects and reports:

1. **Parser Errors**: Syntax errors, unexpected tokens, malformed constructs (shown as ERROR severity)
2. **HIR Compilation Errors**: Type errors, semantic errors (when HIR compilation fails, shown as ERROR severity)

## Code Actions (Quick Fixes)

The language server now provides automatic fixes for common errors:

### Unknown Field Errors
- Uses real AST type information to suggest actual field names from record/resource types
- Provides quick-fix actions to replace the field name with correct alternatives

### Keyword Errors  
- Maps keywords from other programming languages to WTF equivalents:
  - `def`, `fn`, `fun`, `function` → `func`
  - `struct` → `record`
  - `class` → `resource`, `record`
  - `union` → `variant`
  - `const` → `let`
  - `mut`, `mutable` → `var`
  - `elif`, `elseif` → `else if`
  - `switch`, `case` → `match`
- Provides quick-fix actions to correct the keyword

## Auto-completion

The language server provides intelligent method completion when typing "." after an expression:

### UFCS (Uniform Function Call Syntax) Support
- Any function where the first parameter matches the type can be called as a method
- Example: `func get_name(p: Person) -> String` can be called as `person.get_name()`

### Structural Typing Support
- For record types, methods that work with structurally compatible types are suggested
- A method expecting `{name: String}` will be available on any record with at least a `name: String` field

### Resource Methods
- Direct methods defined on resource types are automatically suggested
- Full method signatures are shown with parameter names and return types

### Example

```wtf
record Person {
    name: String,
    age: Int,
    email: String
}

func get_name_length(person: Person) -> Int {
    person.name.length()
}

func format_contact(p: Person) -> String {
    "${p.name} <${p.email}>"
}

func main() {
    let person = Person {
        name: "Alice",
        age: 30,
        email: "alice@example.com"
    }
    
    // Typing "person." will suggest:
    // - get_name_length(person: Person) -> Int
    // - format_contact(p: Person) -> String
    let result = person.■
}
```

## Development

Run the language server in development mode:

```bash
cargo run -p wtf-lsp
```

Test with error detection:

```bash
cd test-lsp
cargo run
```

## Architecture

The language server is built on:

- **tower-lsp**: LSP protocol implementation
- **wtf-parser**: WTF language parser for syntax analysis
- **wtf-hir**: High-level IR compiler for semantic analysis
- **wtf-error**: Error types and formatting

## Examples

### Valid WTF Code

```wtf
func main() -> s32 {
    let x = 42
    return x
}

record person {
    name: string
    age: s32
}
```

### Code with Errors

```wtf
func broken() {
    let y = "hello"
    some_invalid_token$  // ← Syntax error here
    return y
}
```

The language server will highlight the `$` character and provide a diagnostic message about the unexpected token.