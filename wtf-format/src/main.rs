use std::{fs, path::PathBuf};

use wtf_format::FormatPrint;
use wtf_parser::parser::Parser;

pub fn main() -> std::io::Result<()> {
    let args = std::env::args().skip(1);
    let mut paths: Vec<_> = args.map(|arg| PathBuf::from(arg)).collect();

    if paths.is_empty() {
        paths = fs::read_dir(".")?
            .map(|rd| rd.map(|d| d.path()))
            .collect::<Result<Vec<_>, _>>()?;
    }

    for path in paths {
        let file = fs::read_to_string(&path)?;
        let mut parser = Parser::new(&file);
        let module = parser.parse_module().expect("Invalid WTF file!");

        println!("==== {} ====", path.to_string_lossy());
        println!("{}", module.format_print(0));
        println!();
    }

    Ok(())
}
