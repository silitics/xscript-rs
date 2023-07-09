<h1 align="center">
  XScript
</h1>
<h4 align="center">
  A library for writing robust shell-script-like programs and running commands anywhere with ease.
</h4>
<p align="center">
  <a href="https://crates.io/crates/xscript"><img alt="XScript Crate" src="https://img.shields.io/crates/v/xscript"></a>
  <a href="https://docs.rs/xscript/latest/xscript/"><img alt="Docs" src="https://img.shields.io/static/v1?label=docs&message=docs.rs&color=blue"></a>
  <a href="https://crates.io/crates/xscript"><img alt="License: MIT/Apache" src="https://img.shields.io/crates/l/xscript"></a>
</p>

```rust
use xscript::{read_str, run, vars, EnvRun, LocalEnv};

let mut env = LocalEnv::current_dir()?.with_vars(vars! {
    RUSTDOCFLAGS = "--cfg docsrs --cfg xscript_unstable",
    RUSTFLAGS = "--cfg xscript_unstable",
});

let project_root = read_str!(env, ["git", "rev-parse", "--show-toplevel"])?;
env.change_dir(project_root)?;

let cargo_args = ["+nightly"];
let doc_args = ["--lib", "--all-features"];
run!(env, ["cargo", ...cargo_args, "doc", ...doc_args])?;
```

Checkout the [documentation](https://docs.rs/xscript/latest/xscript/) for details.

## ⚖️ Licensing

This project is licensed under either [MIT](LICENSE-MIT) or [Apache 2.0](LICENSE-APACHE) at your opinion. Unless you explicitly state otherwise, any contribution intentionally submitted for inclusion in this project by you, as defined in the Apache 2.0 license, shall be dual licensed as above, without any additional terms or conditions.

---

Made with ❤️ for OSS by [Silitics](https://www.silitics.com).
