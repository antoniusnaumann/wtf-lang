use wtf_parser::parser::Parser;

fn main() {
    let test_code = r#"
// Test WTF file with errors for language server testing

func main() -> s32 {
    let x = 42
    return x
}

// This should have a syntax error - invalid token
func broken() {
    let y = "hello"
    some_invalid_token$
    return y
}

record person {
    name: string
    age: s32
}
"#;

    println!("Testing parser with error-containing code:");
    println!("{}", test_code);
    println!("{}", "=".repeat(50));

    let mut parser = Parser::new(test_code);
    let parse_result = parser.parse_module();
    let source_chars = parser.chars();

    println!("Parse result: {:?}", parse_result.is_ok());
    println!("Parser errors: {}", parser.errors().len());

    for (i, error) in parser.errors().iter().enumerate() {
        println!("Error {}: {:?}", i + 1, error);
        println!("  Message: {}", error.with_source(source_chars));
    }

    if let Err(error) = parse_result {
        println!("Main parse error: {:?}", error);
        println!("  Message: {}", error.with_source(source_chars));
    }
    
    println!("\n{}", "=".repeat(50));
    println!("Testing position calculation...");
    
    // Test our position calculation logic
    let source_chars: Vec<char> = test_code.chars().collect();
    println!("Source length: {} chars", source_chars.len());
    
    // Find the position of the error token '$'
    for (i, &ch) in source_chars.iter().enumerate() {
        if ch == '$' {
            let pos = byte_to_position(i, &source_chars);
            println!("Found '$' at byte {} -> line {}, character {}", i, pos.0, pos.1);
        }
    }
}

fn byte_to_position(byte_offset: usize, chars: &[char]) -> (u32, u32) {
    let mut line = 0;
    let mut character = 0;
    
    for (i, &ch) in chars.iter().enumerate() {
        if i >= byte_offset {
            break;
        }
        if ch == '\n' {
            line += 1;
            character = 0;
        } else {
            character += 1;
        }
    }
    
    (line, character)
}