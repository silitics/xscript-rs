Run commands asynchronously in a Docker container.

Currently we use [`docker_api`] to interface with Docker.
Under the hood, `docker_api` uses Tokio, i.e., to use this environment, you have to run everything with an active Tokio runtime.