use std::{error::Error, time::Duration};

use xscript::{cmd, docker::Docker};

#[cfg(target_family = "unix")]
#[tokio::main]
pub async fn main() -> Result<(), Box<dyn Error>> {
    println!("Connecting to Docker...");
    let docker = Docker::new("unix:///var/run/docker.sock").await?;

    println!("Starting a container...");
    let env = docker.start_container("rust:latest").await?;

    println!("Running cargo...");
    let result = env.run(&cmd!("cargo", "--version")).await;

    match result {
        Ok(output) => {
            println!(
                "{}",
                std::str::from_utf8(output.stdout.as_ref().unwrap()).unwrap()
            );
        }
        Err(error) => {
            eprintln!("{:?}", error);
            eprintln!("{}", error);
        }
    }

    drop(env);

    // Dropping the `env` will remove the temporary container asynchronously.
    // Give it some time...
    tokio::time::sleep(Duration::from_secs(5)).await;
    Ok(())
}

#[cfg(not(target_family = "unix"))]
pub fn main() {
    panic!("Example only runs on Unix.")
}
