use syntect::parsing::SyntaxSet;
use syntect::dumps::dump_to_file;
use std::path::Path;

fn main() {
    // Load default syntaxes
    let mut builder = SyntaxSet::load_defaults_newlines().into_builder();

    // Add custom syntaxes if any
    if Path::new("syntaxes").exists() {
        builder.add_from_folder("syntaxes", true).unwrap();
    }

    // Build the syntax set
    let syntax_set = builder.build();

    // Dump to a binary file
    dump_to_file(&syntax_set, "packaged-syntaxes.packdump").unwrap();
}