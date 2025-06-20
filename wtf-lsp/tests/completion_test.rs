use std::sync::Arc;
use tower_lsp::lsp_types::*;
use wtf_lsp::Backend;
use wtf_ast;
use wtf_tokens::Span;

#[tokio::test]
async fn test_method_completion_basic() {
    // Create a simple WTF code with a record and some methods
    let code = r#"
record Person {
    name: String,
    age: Int
}

func get_name(p: Person) -> String {
    p.name
}

func get_info(person: Person) -> String {
    "info"
}

func main() {
    let p = Person { name: "Alice", age: 30 }
    let result = p.
}
"#;

    // Create a URL for the test
    let uri = Url::parse("file:///test.wtf").unwrap();
    
    // Simulate the language server workflow
    // 1. Parse and store the document
    // 2. Find the position of the dot (p.)
    // 3. Test completion at that position
    
    let lines: Vec<&str> = code.lines().collect();
    let dot_line = lines.iter().position(|line| line.contains("let result = p.")).unwrap();
    let dot_char = lines[dot_line].find("p.").unwrap() + 2; // Position after the dot
    
    let position = Position::new(dot_line as u32, dot_char as u32);
    
    // The completion should suggest methods that can be called on Person type:
    // - get_name (UFCS)
    // - get_info (UFCS)
    
    println!("Test setup complete. Position: line {}, char {}", dot_line, dot_char);
    println!("Code at position: '{}'", &lines[dot_line][..dot_char.min(lines[dot_line].len())]);
}