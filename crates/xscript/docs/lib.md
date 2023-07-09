# XScript

A library for writing robust shell-script-like programs and running commands anywhere with ease.

The initial motivation for this crate was [`xtask`](https://github.com/matklad/cargo-xtask) development.
Taken from the [`xtask` of this crate](https://github.com/silitics/xscript-rs/blob/main/xtask/), here is an [example for building the documentation you are currently reading](https://github.com/silitics/xscript-rs/blob/main/crates/xscript-examples/examples/build-docs.rs) with the nightly toolchain and proper `--cfg` flags:

```rust
use xscript::{read_str, run, vars, Run, LocalEnv};

let mut env = LocalEnv::current_dir()?.with_vars(vars! {
    RUSTDOCFLAGS = "--cfg docsrs --cfg xscript_unstable",
    RUSTFLAGS = "--cfg xscript_unstable",
});

let project_root = read_str!(env, ["git", "rev-parse", "--show-toplevel"])?;
env.change_dir(project_root)?;

let cargo_args = ["+nightly"];
let doc_args = ["--lib", "--all-features"];
run!(env, ["cargo", ...cargo_args, "doc", ...doc_args])?;
# Result::<(), Box<dyn std::error::Error>>::Ok(())
```

🙏 **Acknowledgements**: The design of this crate has been heavily inspired by [`xshell`](https://docs.rs/xshell/latest/xshell/).
See [Related Crates](#related-crates) for details.

🤔 **Rationale**: Executing commands via [`std::process::Command`](https://doc.rust-lang.org/std/process/struct.Command.html) can be quite cumbersome.
You have to be careful to check the exit status of the child process as otherwise errors may go unnoticed, you have to take care of the input and output manually, and you have to prepare the environment and set the working directory for each command individually.[^1]
While this fine-grained control is the correct choice for the standard library – and, in fact, an explicit goal – this crate aims to provide more convenient APIs for common shell-script-like use cases where multiple commands are executed in succession.

🎯 **Goals and Non-Goals**: This crate aims to provide convenient and hard-to-misuse APIs for writing robust shell-script-like programs where multiple commands are executed in succession and in the same environment.
For this purpose, an environment may also be a Docker container or a remote machine.
This crate does not aim to be a general-purpose solution for all command launching needs, though, and instead favors ease of use for common use cases.

[^1]: Or, modify the global environment of the parent process.

## User Guide

Let's first establish some base terminology and corresponding types:

- A _command_ consists of a _program_ and _arguments_.
  Commands are represented by the [`Cmd`] type.
  In addition to the program and its arguments, the [`Cmd`] type may also hold further information, e.g., the `stdin` input to provide.
- An _environment_ provides a context in which commands can be executed.
  Usually, it consists of a _working directory_ and values for _environment variables_.
  [`LocalEnv`] is an environment for executing commands locally.

This separation of commands and environments is central to shell-script-like programs.
It enables the convenient execution of multiple commands in the same environment without rebuilding it all the time.

### Commands

For convenient command construction, the [`cmd!`] macro is provided.
The first argument of the [`cmd!`] macro becomes the program of the command.
The remaining arguments become the arguments of the command.
Here is a simple example:

```rust
# use xscript::cmd;
cmd!("git", "rev-parse", "--show-toplevel");
```

When using string literals, the program and arguments both support string interpolation with [`format!`]:

```rust
# use xscript::cmd;
let prefix = "/usr/bin";
let user = "silitics";
let repo = "xscript-rs";
cmd!("{prefix}/git", "clone", "https://github.com/{user}/{repo}.git");
```

Instead of string literals, any expression implementing [`AsRef<str>`] can be used for the program and its arguments.
Further, to extend the arguments of a command with an iterable, any iterable expression can be prefixed with `...`:

```rust
# use xscript::cmd;
const CARGO: &str = "cargo";
let cargo_command = "doc";
let cargo_args = ["+nightly"];
let extra_args = ["--lib", "--all-features"];
cmd!(CARGO, ...cargo_args, cargo_command, ...extra_args);
```

After constructing a command with [`cmd!`], builder methods can be called to further configure it (see [`Cmd`] for details).
For instance, environment variables can be set per command and the `stdin` input can be specified:

```rust
# use xscript::{cmd};
cmd!("cargo", "+nightly", "doc").with_var("RUSTDOCFLAGS", "--cfg docsrs");
cmd!("gzip", "--best").with_stdin("Compress this string!");
```

Note that [`cmd!`] merely constructs the command but does not yet execute it.

**Remarks**:
In contrast to the standard library's [`Command`][std::process::Command], the program and arguments must be proper UTF-8 strings.
Non UTF-8 strings are a pain to work with and non-portable.
We want [`Cmd`] to be portable in the sense that it is easy to serialize and convert to a proper UTF-8 string without losing any information (e.g., for logging and debugging).
An unfortunate consequence of this design is that [`Path`][std::path::Path] cannot directly be used as an argument.
The recommended way to deal with paths is to use [`camino`](https://docs.rs/camino/latest/camino/).

### Environments

Commands are executed in environments.
Note that we expressly intend for third-party crates to implement environments, e.g., for running commands on a remote machine via SSH.
Behind an unstable feature flag (see [Optional Features](#optional-features)), this crate also provides an experimental environment for running commands in a Docker container.

Environments should implement the [`Run`] and/or the [`RunAsync`] trait.

**Sane I/O Defaults**: Unless otherwise specified as part of a command, environments should set `stdin` to [`In::Null`] by default.
Inheriting `stdin` is dangerous for two reasons, (a) a command may wait for input and, thus, block the main program, and (b) a command may interfere with the main program.
Likewise, by default, all outputs should be captured, i.e., `stdout` and `stderr` should be set to [`Out::Capture`] by default.
This means, that they are both available as part of [`RunError`] or [`RunOutput`] independently of whether the program succeeds or fails.
The `stderr` output should also be _replayed_ by default, i.e., written to the parent process' `stderr` stream (after the command terminated).

### Running Commands

After constructing a command, the methods of the [`Run`] ([`RunAsync`]) trait can be used to run it.
For convenience, the macros [`run!`], [`read_str!`], and [`read_bytes!`] are provided combining the construction and execution.
For instance

```rust
# use xscript::{read_str, Run, LocalEnv};
#
# let env = LocalEnv::current_dir()?;
read_str!(env, ["git", "rev-parse", "--show-toplevel"])?;
# Result::<(), Box<dyn std::error::Error>>::Ok(())
```

runs `git rev-parse --show-toplevel` (which outputs a Git repository's root directory on `stdout`) and reads its `stdout` output as a string.
Here, everything inside the brackets `[...]` is forwarded to the [`cmd!`] macro (see [Commands](#commands)).
The syntax of [`run!`] and [`read_bytes!`] is analogous.
In addition, calls to [`Cmd`] builder methods can simply be appended:

```rust
# use xscript::{read_bytes, Run, LocalEnv};
#
# let env = LocalEnv::current_dir()?;
read_bytes!(env, ["gzip", "--best"].with_stdin("Compress this string!"))?;
# Result::<(), Box<dyn std::error::Error>>::Ok(())
```

## Optional Features

This crate offers the following optional features:

- `serde`: Support for serialization and deserialization of various data structures via [Serde][serde].

🚧 In addition, there are also the following _unstable_ work-in-progress features:

- `async`: Support for asynchronous environments (in particular, the [`RunAsync`] trait).
- `docker`: Support for running commands asynchronously in a Docker container.
- `tokio`: Support for running commands asynchronously using [Tokio][tokio].

⚠️ **Warning**: Unstable means that they may not even be in a working state.
They require `--cfg xscript_unstable`.

## Related Crates

This crate is by far not the first aiming to simplify command launching.
The crates [`xshell`](https://docs.rs/xshell/latest/xshell/), [`devx-cmd`](https://docs.rs/devx-cmd/latest/devx_cmd/), and [`duct`](https://docs.rs/duct/latest/duct/) start with the same motivation.
In terms of both, its design and its goals, `xshell` comes closest to `xscript`.
In contrast to `devx-cmd` and `duct`, `xshell` and `xscript` aim to separate the execution environment from the commands being launched.
This separation is central to shell-script-like use cases because it enables launching multiple commands in the same environment.

In case of `xshell`, the [`Shell`](https://docs.rs/xshell/latest/xshell/struct.Shell.html) type provides a common environment for launching multiple commands.
Just like a shell, it has a current working directory in which commands are launched and defines environment variables.
Note, however, that `xshell` still couples every command to a `Shell`.
Now, `xscript` follows the same idea but its command type [`Cmd`] is completely decoupled from the execution environment.
Instead of calling a method on the command to launch it, the idea is to provide the command as an argument to a method of an environment.
An environment then defines the exact way in which commands are launched.
For instance, they may launch the command in a Docker container or via SSH on a remote machine, and they may offer asynchronous launch methods.
Crucially, environments can also be defined by third-party crates.
Another technical difference between `xshell` and `xscript` is that `xshell` uses a proc macro and parses a command provided as a string while `xscript` does not do any string parsing but requires the arguments to be given individually.
In contrast to `xscript`, `xshell`'s `Shell` environment also offers methods to work with the filesystem.
We may also add such functionality to `xscript` later.

Note that `xscript` does not support building pipelines where the output of one command is continuously fed as an input to another command running at the same time.
In case you need pipelines, consider using [`duct`](https://docs.rs/duct/latest/duct/) instead.
