use glob::glob;
use std::env;
use std::path::PathBuf;
use std::process::Command;

fn main() {
    // Build the foreign library with Cabal
    run_cabal();

    // Generate bindings from cbits/Lib.h
    run_bindgen();
}

fn run_cabal() {
    // Get the output directory
    let out_dir = PathBuf::from(env::var("OUT_DIR").unwrap());

    // Tell cargo to rebuild if the .cabal or any .hs files change
    println!("cargo:rerun-if-changed={}", "lake-generator.cabal");
    for entry in glob("**/*.hs").expect("Failed to read glob pattern") {
        match entry {
            Ok(source_file) => {
                println!("cargo:rerun-if-changed={}", source_file.to_str().unwrap())
            }
            Err(e) => {
                println!("cargo:warning={}", e)
            }
        }
    }

    // Build the foreign library with Cabal
    let opt_builddir = format!(
        "--builddir={}",
        out_dir.join("dist-newstyle").to_str().unwrap()
    );
    Command::new("cabal")
        .args(["v2-build", &opt_builddir])
        .status()
        .unwrap();

    // Tell cargo to link against the compiled Haskell library
    println!("cargo:rustc-link-search={}", out_dir.to_str().unwrap());
    println!("cargo:rustc-link-lib=lake-generator");
}

fn run_bindgen() {
    // Tell cargo to rerun if the header file changes
    let header = PathBuf::from("cbits").join("Lib.h");
    let header_str = header.to_str().unwrap();
    println!("cargo:rerun-if-changed={}", header_str);

    // Generate the bindings from cbits/Lib.h
    let bindings = bindgen::Builder::default()
        .header(header_str)
        .generate()
        .expect("Unable to generate bindings");

    // Write the bindings to the $OUT_DIR/bindings.rs file.
    let out_dir = PathBuf::from(env::var("OUT_DIR").unwrap());
    bindings
        .write_to_file(out_dir.join("bindings.rs"))
        .expect("Couldn't write bindings!");
}
