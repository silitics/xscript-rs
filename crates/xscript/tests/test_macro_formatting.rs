use std::ffi::OsString;

use xscript::{run, Cmd, Run, RunError, RunOutput};

pub struct FakeEnv;

impl Run<OsString> for FakeEnv {
    fn run(&self, _: Cmd<OsString>) -> Result<RunOutput, RunError<OsString>> {
        Ok(RunOutput::new()
            .with_code(0)
            .with_stdout("".into())
            .with_stderr("".into()))
    }
}

#[test]
pub fn test_macro_formatting() {
    let env = FakeEnv;

    run!(
        env,
        [
            "this", "is", "a", "very", "long", "command", "that", "gets", "even", "longer", "to",
            "see", "how", "rustfmt", "breaks", "it", "and", "just", "for", "the", "fun", "of",
            "it", "we", "add", "even", "more", "parts"
        ]
    )
    .unwrap();

    run!(
        env,
        [
            "this", "is", "a", "very", "long", "command", "that", "gets", "even", "longer", "to",
            "see", "how", "rustfmt", "breaks", "it", "and", "just", "for", "the", "fun", "of",
            "it", "we", "add", "even", "more", "parts"
        ]
        .with_stdin("This is some input!")
        .with_var("some_var", "some_value")
    )
    .unwrap();

    run!(
        env,
        ["this", "is", "a", "command"]
            .with_stdin("This is some input!")
            .with_var("some_var", "some_value")
            .allow_failures()
    )
    .unwrap();

    run!(
        env,
        [
            "what happens if the individual",
            "parts are a bit longer?",
            "how does rustfmt format them?"
        ]
        .with_stdin("This is some input!")
        .with_var("some_var", "some_value")
        .allow_failures()
    )
    .unwrap();
}
