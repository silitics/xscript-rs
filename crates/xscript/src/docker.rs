#![doc = include_str!("../docs/docker.md")]
#![allow(dead_code)] // This is work in progress.

use std::io;

use docker_api::opts::{ContainerCreateOpts, ContainerRemoveOpts, ExecCreateOpts, ExecStartOpts};
use futures::{AsyncWriteExt, StreamExt};
use thiserror::Error;

use crate::{Cmd, In, RunError, RunErrorKind, RunOutput, RunResult};

#[derive(Debug, Error)]
#[error(transparent)]
pub struct DockerError(docker_api::Error);

pub struct Docker(docker_api::Docker);

impl Docker {
    pub async fn new<U: AsRef<str>>(uri: U) -> Result<Self, DockerError> {
        docker_api::Docker::new(uri).map(Self).map_err(DockerError)
    }

    pub fn container_env<I: AsRef<str>>(&self, id: I) -> ContainerEnv {
        ContainerEnv::new(self.0.clone(), id.as_ref().to_owned(), false)
    }

    pub async fn start_container(
        &self,
        image: impl AsRef<str>,
    ) -> Result<ContainerEnv, DockerError> {
        let container = self
            .0
            .containers()
            .create(
                &ContainerCreateOpts::builder()
                    .image(image.as_ref())
                    .entrypoint(["sleep", "infinity"])
                    .build(),
            )
            .await
            .map_err(DockerError)?;
        container.start().await.map_err(DockerError)?;
        Ok(ContainerEnv::new(
            self.0.clone(),
            container.id().to_string(),
            true,
        ))
    }
}

pub struct ContainerEnv {
    docker: docker_api::Docker,
    id: String,
    cwd: Option<String>,
    privileged: Option<bool>,
    user: Option<String>,
    group: Option<String>,
    remove_on_drop: bool,
}

impl ContainerEnv {
    fn new(docker: docker_api::Docker, id: String, remove_on_drop: bool) -> Self {
        Self {
            docker,
            id,
            cwd: None,
            privileged: None,
            user: None,
            group: None,
            remove_on_drop,
        }
    }

    pub async fn run(&self, cmd: &Cmd<String>) -> RunResult<RunOutput, String> {
        RunError::catch_async(cmd, || {
            async {
                let create_opts = ExecCreateOpts::builder()
                    .command(std::iter::once(cmd.prog()).chain(cmd.args()))
                    .attach_stdout(true)
                    .attach_stderr(true)
                    .build();

                let exec =
                    docker_api::exec::Exec::create(self.docker.clone(), &self.id, &create_opts)
                        .await
                        .map_err(RunErrorKind::other)?;

                let start_opts = ExecStartOpts::builder().build();

                let (mut rx, mut tx) = exec
                    .start(&start_opts)
                    .await
                    .map_err(RunErrorKind::other)?
                    .split();

                let mut stdout = Vec::new();
                let mut stderr = Vec::new();

                let write_stdin_fut = async {
                    if let Some(In::Bytes(bytes)) = cmd.stdin() {
                        tx.write_all(bytes).await?;
                    }
                    tx.flush().await?;
                    tx.close().await?;
                    drop(tx);
                    Result::<(), io::Error>::Ok(())
                };

                let read_output_fut = async {
                    while let Some(chunk) = rx.next().await {
                        let chunk = chunk?;
                        use docker_api::conn::TtyChunk;
                        match chunk {
                            TtyChunk::StdIn(_) => {
                                // Can this even happen?!?
                            }
                            TtyChunk::StdOut(bytes) => {
                                stdout.extend(bytes);
                            }
                            TtyChunk::StdErr(bytes) => {
                                stderr.extend(bytes);
                            }
                        }
                    }
                    Result::<(), docker_api::Error>::Ok(())
                };

                let (write_result, read_result) = futures::join!(write_stdin_fut, read_output_fut);
                read_result.map_err(RunErrorKind::other)?;
                write_result.map_err(RunErrorKind::other)?;

                let inspect = exec.inspect().await.map_err(RunErrorKind::other)?;

                let code = inspect.exit_code.map(|code| code as i32);

                let output = RunOutput {
                    code,
                    stdout: Some(stdout),
                    stderr: Some(stderr),
                };

                if code == Some(0) {
                    Ok(output)
                } else {
                    Err(RunErrorKind::Failed(output))
                }
            }
        })
        .await
    }
}

impl Drop for ContainerEnv {
    fn drop(&mut self) {
        if self.remove_on_drop {
            let docker = self.docker.clone();
            let container_id = self.id.clone();
            tokio::spawn(async move {
                let container = docker_api::Container::new(docker, container_id);
                let _ = container
                    .remove(&ContainerRemoveOpts::builder().force(true).build())
                    .await;
            });
        }
    }
}
