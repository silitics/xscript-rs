use std::env;
use std::path::PathBuf;
use std::sync::OnceLock;

use clap::Parser;
use xscript::{read_str, run, vars, LocalEnv, Out, Run};

#[derive(Debug, Parser)]
pub struct Args {
    #[clap(subcommand)]
    task: Task,
}

#[derive(Debug, Parser)]
pub enum Task {
    /// Generate the documentation.
    Doc(DocArgs),
    /// Lint the source code.
    Lint,
    /// Run tests.
    Test,
}

#[derive(Debug, Parser)]
pub struct DocArgs {
    /// Extra arguments passed through to `cargo doc`.
    #[clap(allow_hyphen_values(true))]
    extra_args: Vec<String>,
}

fn main() -> anyhow::Result<()> {
    let args = Args::parse();
    init_ctx()?;
    match &args.task {
        Task::Doc(args) => task_doc(args)?,
        Task::Lint => task_lint()?,
        Task::Test => task_test()?,
    }
    Ok(())
}

fn task_doc(args: &DocArgs) -> anyhow::Result<()> {
    run!(
        env(),
        [
            "cargo", "+nightly", "doc", "--quiet", "--message-format=short",
            "--color=always", "--lib", "--all-features", ...&args.extra_args
        ].with_vars(vars! {
            RUSTDOCFLAGS = "--cfg docsrs",
        })
    )?;
    Ok(())
}

fn task_lint() -> anyhow::Result<()> {
    let env = env();
    run!(
        env,
        [
            "cargo",
            "check",
            "--quiet",
            "--message-format=short",
            "--color=always",
        ]
    )?;
    run!(
        env,
        [
            "cargo",
            "clippy",
            "--quiet",
            "--message-format=short",
            "--color=always",
        ]
    )?;
    run!(
        env,
        [
            "cargo",
            "+nightly",
            "fmt",
            "--check",
            "--message-format=short",
            "--",
            "--color=always",
        ]
    )?;
    Ok(())
}

fn task_test() -> anyhow::Result<()> {
    let env = env();
    run!(env, ["cargo", "test"].with_stderr(Out::Inherit))?;
    Ok(())
}

pub struct Context {
    pub invocation_dir: PathBuf,
    pub project_dir: PathBuf,
    pub project_env: LocalEnv,
}

static CTX: OnceLock<Context> = OnceLock::new();

fn init_ctx() -> anyhow::Result<()> {
    let mut env = LocalEnv::current_dir()?.with_echo();
    let git_root = read_str!(env, ["git", "rev-parse", "--show-toplevel"])?;
    env.change_dir(&git_root)?;
    CTX.set(Context {
        project_env: env,
        invocation_dir: env::current_dir()?,
        project_dir: git_root.into(),
    })
    .map_err(|_| anyhow::anyhow!("Context has already been initialized."))
}

pub fn ctx() -> &'static Context {
    CTX.get().expect("Context has not been initialized.")
}

pub fn env() -> &'static LocalEnv {
    &ctx().project_env
}
