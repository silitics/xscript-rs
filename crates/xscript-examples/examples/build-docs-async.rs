use std::error::Error;

use xscript::{read_str, run, vars, LocalEnv, RunAsync};

#[tokio::main]
pub async fn main() -> Result<(), Box<dyn Error>> {
    let mut env = LocalEnv::current_dir()?.with_vars(vars! {
        RUSTDOCFLAGS = "--cfg docsrs --cfg xscript_unstable",
        RUSTFLAGS = "--cfg xscript_unstable",
    });

    let project_root = read_str!(env, ["git", "rev-parse", "--show-toplevel"]).await?;
    env.change_dir(project_root)?;

    let cargo_args = ["+nightly"];
    let doc_args = ["--lib", "--all-features"];
    run!(env, ["cargo", ...cargo_args, "doc", ...doc_args]).await?;

    Ok(())
}
