use std::os::unix::ffi::OsStrExt as _;
use std::path::PathBuf;
use std::process::Command;

#[test]
fn test_expand() {
    macrotest::expand("tests/expand/*.rs");
}

#[test]
fn test_compile() {
    macrotest::expand("tests/compile/*.rs");

    let typle_package = format!("typle-{}", std::env::var("CARGO_PKG_VERSION").unwrap());
    let mut lib_path = PathBuf::from(std::env::var_os("CARGO_MANIFEST_DIR").unwrap().to_owned());
    lib_path.extend([
        "target",
        "package",
        &typle_package,
        "target",
        "debug",
        "libtyple.so",
    ]);
    for path in std::fs::read_dir("tests/compile").unwrap() {
        let entry = path.unwrap();
        let file_name = entry.file_name();
        if let Some(fname) = file_name.to_str() {
            if fname.ends_with(".rs") && !fname.ends_with(".expanded.rs") {
                // Compile source code
                let status = Command::new("rustc")
                    .args([
                        std::str::from_utf8(entry.path().as_os_str().as_bytes()).unwrap(),
                        "--edition",
                        "2021",
                        "--crate-name",
                        "test",
                        "--crate-type",
                        "lib",
                        "--allow",
                        "dead_code",
                        "--extern",
                        &format!("typle={}", lib_path.to_str().unwrap()),
                    ])
                    .status()
                    .unwrap();
                assert!(status.success())
            }
        }
    }
}
