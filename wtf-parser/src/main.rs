use wtf_parser::lexer::{Lexer, Token};

fn main() {
    let input = r#"
    package documentation:http@1.0.0;

    use wasi:clocks/wall-clock.{datetime}

    record point {
        x: s32
        y: s32
    }

    func add(a: s32, b: s32) -> s32 {
        a + b
    }

    let p1 = point {
        x: 5,
        y: 10
    }

    var p2 = point {
        x: 5,
        y: 10
    }

    p2.x = 15
    "#;

    let mut lexer = Lexer::new(input);

    loop {
        let spanned_tok = lexer.next_token();
        println!("{:?} at {:?}", spanned_tok.token, spanned_tok.span);
        if let Token::Eof = spanned_tok.token {
            break;
        }
    }
}
