use xscript::{read_bytes, read_str, run, Run};

#[test]
pub fn test_parent_env_macros() {
    run!(["git", "rev-parse", "--show-toplevel"]).unwrap();
    assert_eq!(
        read_str!(["cat"].with_stdin("Hello World!")).unwrap(),
        "Hello World!"
    );
    assert_eq!(
        read_bytes!(["cat"].with_stdin("Hello World!")).unwrap(),
        b"Hello World!"
    );
}

#[test]
pub fn test_native_paths() {
    let path = std::env::current_dir().unwrap();
    run!(["ls", "-l", path]).unwrap();
}
