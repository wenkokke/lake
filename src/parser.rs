use tree_sitter::Language;

extern "C" {
    pub fn tree_sitter_lake() -> Language;
}

#[test]
fn test_parser() {
    use tree_sitter::Parser;

    let language = unsafe { tree_sitter_lake() };
    let mut parser = Parser::new();
    parser.set_language(language).unwrap();

    let source_code = "hello = world";
    let tree = parser.parse(source_code, None).unwrap();

    assert_eq!(
        tree.root_node().to_sexp(),
        "(source_file (declaration (identifier) (expression (identifier))))"
    );
}
