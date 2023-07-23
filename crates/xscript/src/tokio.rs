//! Run commands asynchronously using [Tokio][tokio].
//!
//! This module adds an implementation of [`RunAsync`] to [`LocalEnv`].

use tokio::{
    io::{self, AsyncWriteExt},
    process::Command,
};

use crate::{Cmd, In, LocalEnv, RunAsync, RunError, RunErrorKind, RunOutput, RunResult, Vars};

impl RunAsync for LocalEnv {
    fn run(&self, cmd: Cmd) -> crate::BoxedFuture<RunResult<RunOutput>> {
        Box::pin(async move {
            let cmd = &cmd;
            let mut command = Command::new(&*self.resolve_prog(cmd.prog()));
            command.args(cmd.args());
            if let Some(cwd) = cmd.cwd() {
                command.current_dir(self.resolve_path(cwd));
            } else {
                command.current_dir(&self.0.cwd);
            }
            // Populate the environment variables.
            if self.vars().is_clean() || cmd.vars().map(|vars| vars.is_clean()).unwrap_or(false) {
                command.env_clear();
            }
            update_vars(&mut command, self.vars());
            if let Some(vars) = cmd.vars() {
                update_vars(&mut command, vars);
            }
            // Configure IO.
            command.stdin(cmd.stdin().unwrap_or_else(|| self.default_stdin()).stdio());
            command.stdout(
                cmd.stdout()
                    .unwrap_or_else(|| self.default_stdout())
                    .stdio(),
            );
            command.stderr(
                cmd.stderr()
                    .unwrap_or_else(|| self.default_stderr())
                    .stdio(),
            );
            // Make sure to kill and (eventually) reap the process when the future is aborted.
            command.kill_on_drop(true);
            RunError::catch_async(cmd, || async move {
                let mut child = command.spawn()?;
                let capture_stdout = child.stdout.is_some();
                let capture_stderr = child.stderr.is_some();

                let stdin = child.stdin.take();
                let write_stdin_fut = async {
                    if let Some(mut stdin) = stdin {
                        if let Some(In::Bytes(bytes)) = cmd.stdin() {
                            stdin.write_all(bytes).await?;
                        }
                        stdin.flush().await?;
                        drop(stdin);
                    }
                    Result::<(), io::Error>::Ok(())
                };

                let (write_result, read_result) =
                    futures::join!(write_stdin_fut, child.wait_with_output());
                let child_output = read_result?;
                write_result?;

                if self.0.replay_stdout {
                    io::stdout().write_all(&child_output.stdout).await.ok();
                }
                if self.0.replay_stderr {
                    io::stderr().write_all(&child_output.stderr).await.ok();
                }
                let output = RunOutput {
                    code: child_output.status.code(),
                    stdout: if capture_stdout {
                        Some(child_output.stdout)
                    } else {
                        None
                    },
                    stderr: if capture_stderr {
                        Some(child_output.stderr)
                    } else {
                        None
                    },
                };
                if child_output.status.success() || cmd.may_fail() {
                    Ok(output)
                } else {
                    Err(RunErrorKind::Failed(output))
                }
            })
            .await
        })
    }
}

fn update_vars(command: &mut Command, vars: &Vars) {
    for (name, value) in vars.values() {
        if let Some(value) = value {
            command.env(name, value);
        } else {
            command.env_remove(name);
        }
    }
}
