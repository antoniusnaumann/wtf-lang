# WTF Language Server (wtf-lsp)

A Language Server Protocol (LSP) implementation for the WTF programming language.

## Features

- **Syntax Error Detection**: Real-time parsing and syntax error reporting
- **HIR Error Detection**: High-level intermediate representation compilation errors
- **Document Synchronization**: Support for opening, editing, and closing WTF files
- **Position Mapping**: Accurate mapping of byte offsets to line/character positions

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
- `textDocument/codeAction` (new: provides quick fixes)

## Error Types

The language server detects and reports:

1. **Parser Errors**: Syntax errors, unexpected tokens, malformed constructs (shown as ERROR severity)
2. **HIR Compilation Errors**: Type errors, semantic errors (when HIR compilation fails, shown as ERROR severity)

## Code Actions (Quick Fixes)

The language server now provides automatic fixes for common errors:

### Unknown Field Errors
- Suggests common field names when accessing non-existent fields
- Provides quick-fix actions to replace the field name

### Keyword Errors  
- Maps keywords from other programming languages to WTF equivalents:
  - `def`, `fn`, `fun`, `function` → `func`
  - `struct` → `record`
  - `class` → `resource`, `record`
  - `union` → `variant`
  - `const` → `let`
  - `mut`, `mutable` → `var`
- Extracts expected keywords from error messages
- Provides quick-fix actions to correct the keyword

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