use std::env;
use std::fs;
use std::path::Path;
use std::path::PathBuf;
use walkdir::{DirEntry, WalkDir};

fn main() {
    ts_build();
}

// Build tree-sitter-lake

// The name of the language.
const LANGUAGE: &str = "lake";

// See DEFAULT_GENERATE_ABI_VERSION in tree-sitter-cli:
// https://github.com/tree-sitter/tree-sitter/blob/ab09ae20d640711174b8da8a654f6b3dec93da1a/cli/src/main.rs#L18
const DEFAULT_GENERATE_ABI_VERSION: usize = 14;

fn ts_build() {
    let source_directory = format!("tree-sitter-{}", LANGUAGE);
    let source_directory = Path::new(&source_directory);
    let output_directory = env::var_os("OUT_DIR").unwrap();
    let output_directory = Path::new(&output_directory);
    ts_copy(&source_directory, &output_directory);
    let package_directory = output_directory.join(source_directory);
    ts_generate(&package_directory);
    ts_compile(&package_directory);
}

fn ts_copy(source_directory: &Path, target_directory: &Path) {
    for source_entry in WalkDir::new(source_directory)
        .into_iter()
        .filter_entry(|e| !ts_copy_skip(e))
    {
        let source_entry = source_entry.unwrap();
        let target_entry = target_directory.join(source_entry.path());
        // If src_entry is a directory, create the directory in OUT_DIR.
        if source_entry.file_type().is_dir() && !target_entry.exists() {
            fs::create_dir(target_directory.join(source_entry.path())).unwrap();
        }
        // If src_entry is a file, copy the file to OUT_DIR.
        else if source_entry.file_type().is_file() {
            fs::copy(source_entry.path(), target_entry).unwrap();
        }
    }
}

fn ts_copy_skip(entry: &DirEntry) -> bool {
    entry
        .file_name()
        .to_str()
        .map(|s| ["target", "node_modules"].contains(&s))
        .unwrap_or(false)
}

fn ts_generate(repo_path: &PathBuf) {
    let grammar_path = repo_path.join("grammar.js");
    let grammar_path = grammar_path.to_str().unwrap();
    println!("cargo:rerun-if-changed={}", grammar_path);
    tree_sitter_cli::generate::generate_parser_in_directory(
        &repo_path,
        Some(grammar_path),
        DEFAULT_GENERATE_ABI_VERSION,
        true,
        None,
    )
    .unwrap();
}

fn ts_compile(package_directory: &PathBuf) {
    let source_directory = package_directory.join("src");
    let parser_file = source_directory.join("parser.c");
    let parser_file = parser_file.to_str().unwrap();
    println!("cargo:rerun-if-changed={}", parser_file);
    let scanner_file = source_directory.join("scanner.c");
    let scanner_file = scanner_file.to_str().unwrap();
    println!("cargo:rerun-if-changed={}", scanner_file);
    let output = format!("tree-sitter-{}", LANGUAGE);
    cc::Build::new()
        .file(parser_file)
        .include(source_directory)
        .compile(&output);
}
