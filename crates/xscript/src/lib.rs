#![doc = include_str!("../docs/lib.md")]
#![cfg_attr(docsrs, feature(doc_cfg))]

use std::{
    borrow::{Borrow, Cow},
    collections::HashMap,
    ffi::{OsStr, OsString},
    fmt::Debug,
    fmt::{Display, Write},
    hash::Hash,
    io,
    ops::Deref,
    path::{Path, PathBuf},
    process::Command,
    sync::Arc,
};

use sealed::Sealed;

#[cfg(all(not(xscript_unstable), feature = "docker"))]
compile_error!("The `docker` feature requires `--cfg xscript_unstable`.");

#[cfg(all(not(xscript_unstable), any(features = "async", feature = "tokio")))]
compile_error!("The `async` and `tokio` features require `--cfg xscript_unstable`.");

#[cfg(feature = "docker")]
#[cfg_attr(docsrs, doc(cfg(feature = "docker")))]
pub mod docker;
#[cfg_attr(docsrs, doc(cfg(feature = "tokio")))]
#[cfg(feature = "tokio")]
pub mod tokio;

/// Module for sealing traits.
#[doc(hidden)]
mod sealed {
    use std::ffi::{OsStr, OsString};

    pub trait Sealed {}

    impl Sealed for str {}

    impl Sealed for OsStr {}

    impl Sealed for String {}

    impl Sealed for OsString {}
}

/// Lossy string conversion.
pub trait ToStringLossy: sealed::Sealed {
    /// Convert to string, potentially skipping invalid characters.
    fn to_string_lossy(&self) -> Cow<str>;
}

impl ToStringLossy for str {
    fn to_string_lossy(&self) -> Cow<str> {
        Cow::Borrowed(self)
    }
}

impl ToStringLossy for OsStr {
    fn to_string_lossy(&self) -> Cow<str> {
        OsStr::to_string_lossy(&self)
    }
}

/// A string type that can be used to construct commands.
pub trait CmdString: 'static + Debug + Clone + Default + Eq + Hash + Sealed
where
    Self: AsRef<Self::Str>,
    Self: AsRef<OsStr>,
    Self: Deref<Target = Self::Str>,
    Self: Borrow<Self::Str>,
{
    /// Unsized equivalent for references.
    type Str: ?Sized
        + ToOwned<Owned = Self>
        + AsRef<OsStr>
        + AsRef<Self::Str>
        + Eq
        + Hash
        + ToStringLossy;

    fn from_str(string: &str) -> &Self::Str;
}

impl CmdString for String {
    type Str = str;

    fn from_str(string: &str) -> &Self::Str {
        string
    }
}

impl CmdString for OsString {
    type Str = OsStr;

    fn from_str(string: &str) -> &Self::Str {
        string.as_ref()
    }
}

/// Shared inner data of a command.
#[derive(Debug, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
struct CmdData<S: CmdString> {
    /// The program to run.
    prog: S,
    /// The arguments to run the program with.
    args: Vec<S>,
    /// The directory in which to run the command.
    cwd: Option<S>,
    /// The environment variables to run the command with.
    vars: Option<Vars<S>>,
    /// The `stdin` input to provide to the command.
    stdin: Option<In>,
    /// Indicates what to do with the `stdout` output of the command.
    stdout: Option<Out>,
    /// Indicates what to do with the `stderr` output of the command.
    stderr: Option<Out>,
    /// Indicates whether the command may fail.
    may_fail: bool,
    /// Hints that the command may contain secret information.
    is_secret: bool,
}

impl<S: CmdString> CmdData<S> {
    fn new(prog: S) -> Self {
        Self {
            prog,
            args: Vec::new(),
            cwd: None,
            vars: None,
            stdin: None,
            stdout: None,
            stderr: None,
            may_fail: false,
            is_secret: false,
        }
    }
}

/// A command.
#[derive(Debug, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[must_use]
pub struct Cmd<S: CmdString = OsString>(Arc<CmdData<S>>);

impl<S: CmdString> Cmd<S> {
    /// Creates a new command for the given program.
    pub fn new<P: AsRef<S::Str>>(prog: P) -> Self {
        Cmd(Arc::new(CmdData::new(prog.as_ref().to_owned())))
    }

    /// The program to run.
    pub fn prog(&self) -> &S::Str {
        self.0.prog.as_ref()
    }

    /// The arguments to run the program with.
    pub fn args(&self) -> impl Iterator<Item = &S::Str> {
        self.0.args.iter().map(AsRef::as_ref)
    }

    /// The directory in which to run the command, if any.
    pub fn cwd(&self) -> Option<&S::Str> {
        self.0.cwd.as_deref()
    }

    /// The environment variables to run the command with.
    pub fn vars(&self) -> Option<&Vars<S>> {
        self.0.vars.as_ref()
    }

    /// The `stdin` input to provide to the command.
    pub fn stdin(&self) -> Option<&In> {
        self.0.stdin.as_ref()
    }

    /// Indicates what to do with the `stdout` output of the command.
    pub fn stdout(&self) -> Option<&Out> {
        self.0.stdout.as_ref()
    }

    /// Indicates what to do with the `stderr` output of the command.
    pub fn stderr(&self) -> Option<&Out> {
        self.0.stderr.as_ref()
    }

    /// Indicates whether the command may fail.
    pub fn may_fail(&self) -> bool {
        self.0.may_fail
    }

    /// Hints that the command may contain secret information.
    pub fn is_secret(&self) -> bool {
        self.0.is_secret
    }

    fn data_mut(&mut self) -> &mut CmdData<S> {
        Arc::make_mut(&mut self.0)
    }

    /// Adds an argument to the command.
    pub fn add_arg<A: AsRef<S::Str>>(&mut self, arg: A) -> &mut Self {
        self.data_mut().args.push(arg.as_ref().to_owned());
        self
    }

    /// Extends the arguments of the command.
    pub fn extend_args<A: AsRef<S::Str>, I: IntoIterator<Item = A>>(
        &mut self,
        args: I,
    ) -> &mut Self {
        self.data_mut()
            .args
            .extend(args.into_iter().map(|arg| arg.as_ref().to_owned()));
        self
    }

    /// Sets the directory in which to run the command.
    pub fn with_cwd<P: AsRef<S::Str>>(mut self, cwd: P) -> Self {
        self.data_mut().cwd = Some(cwd.as_ref().to_owned());
        self
    }

    /// Sets the environment variables to run the command with.
    pub fn with_vars(mut self, vars: Vars<S>) -> Self {
        self.data_mut().vars = Some(vars);
        self
    }

    /// Sets an environment variable.
    pub fn with_var<N: AsRef<S::Str>, V: AsRef<S::Str>>(mut self, name: N, value: V) -> Self {
        self.data_mut()
            .vars
            .get_or_insert_with(Vars::new)
            .set(name, value);
        self
    }

    /// Sets the `stdin` input to provide to the command.
    pub fn with_stdin<T: Into<In>>(mut self, stdin: T) -> Self {
        self.data_mut().stdin = Some(stdin.into());
        self
    }

    /// Sets what to do with the `stdout` output of the command.
    pub fn with_stdout(mut self, stdout: Out) -> Self {
        self.data_mut().stdout = Some(stdout);
        self
    }

    /// Sets what to do with the `stderr` output of the command.
    pub fn with_stderr(mut self, stderr: Out) -> Self {
        self.data_mut().stderr = Some(stderr);
        self
    }

    /// Do not return an error when the command fails.
    pub fn allow_failures(mut self) -> Self {
        self.data_mut().may_fail = true;
        self
    }

    /// Mark the command as secret.
    pub fn make_secret(mut self) -> Self {
        self.data_mut().is_secret = true;
        self
    }
}

impl<S: CmdString> AsRef<Cmd<S>> for Cmd<S> {
    fn as_ref(&self) -> &Cmd<S> {
        self
    }
}

impl<S: CmdString> std::fmt::Display for Cmd<S> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.0.is_secret {
            f.write_str("<secret command redacted>")?
        } else {
            write_escaped(f, &self.0.prog.to_string_lossy())?;
            for arg in &self.0.args {
                f.write_char(' ')?;
                write_escaped(f, &AsRef::<S::Str>::as_ref(arg).to_string_lossy())?;
            }
        }
        Ok(())
    }
}

fn write_escaped(f: &mut dyn std::fmt::Write, string: &str) -> std::fmt::Result {
    let quote = string.contains(char::is_whitespace);
    if quote {
        f.write_char('"')?;
    }
    for char in string.chars() {
        match char {
            '\\' => f.write_str("\\\\")?,
            '"' => f.write_str("\\\"")?,
            _ => f.write_char(char)?,
        }
    }
    if quote {
        f.write_char('"')?;
    }
    Ok(())
}

/// Private auxiliary macro. **Not part of the public API!**
#[macro_export]
#[doc(hidden)]
macro_rules! __private_extend_args {
    ($cmd:ident, ) => {};
    ($cmd:ident, $(, $($args:tt)*)?) => {
        $crate::__private_extend_args!($cmd, $($($args)*)*);
    };
    ($cmd:ident, ...$arg:expr $(, $($args:tt)*)?) => {
        $cmd.extend_args($arg);
        $crate::__private_extend_args!($cmd, $($($args)*)*);
    };
    ($cmd:ident, $value:literal $(, $($args:tt)*)?) => {
        $cmd.add_arg(format!($value));
        $crate::__private_extend_args!($cmd, $($($args)*)*);
    };
    ($cmd:ident, $arg:expr $(, $($args:tt)*)?) => {
        $cmd.add_arg($arg);
        $crate::__private_extend_args!($cmd, $($($args)*)*);
    }
}

/// Constructs a command.
///
/// See [crate] documentation for details and examples.
#[macro_export]
macro_rules! cmd {
    ($prog:literal $(, $($args:tt)*)?) => {{
        #[allow(unused_mut)]
        let mut cmd = $crate::Cmd::new(format!($prog));
        $crate::__private_extend_args!(cmd, $($($args)*)*);
        cmd
    }};
    ($prog:expr $(, $($args:tt)*)?) => {{
        #[allow(unused_mut)]
        let mut cmd = $crate::Cmd::new($prog);
        $crate::__private_extend_args!(cmd, $($($args)*)*);
        cmd
    }};
}

/// Constructs a command using [`OsString`] as string type.
#[macro_export]
macro_rules! cmd_os {
    ($($cmd:tt)*) => {{
        let cmd: $crate::Cmd::<::std::ffi::OsString> = $crate::cmd!($($cmd)*);
        cmd
    }};
}

/// Constructs a command using [`String`] as string type.
#[macro_export]
macro_rules! cmd_str {
    ($($cmd:tt)*) => {{
        let cmd: $crate::Cmd::<::std::string::String> = $crate::cmd!($($cmd)*);
        cmd
    }};
}

/// Indicates what to do with the output of a command.
#[derive(Debug, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum Out {
    /// Discard the output.
    Discard,
    /// Inherit the output stream from the parent process.
    Inherit,
    /// Capture the output.
    Capture,
}

impl Out {
    fn stdio(&self) -> std::process::Stdio {
        match self {
            Out::Discard => std::process::Stdio::null(),
            Out::Inherit => std::process::Stdio::inherit(),
            Out::Capture => std::process::Stdio::piped(),
        }
    }
}

/// An input provided to a command.
#[derive(Debug, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum In {
    /// Do not provide any input, i.e., `/dev/null`.
    Null,
    /// Inherit the input stream from the parent process.
    Inherit,
    /// Provide the given bytes as input.
    Bytes(Vec<u8>),
}

impl In {
    fn stdio(&self) -> std::process::Stdio {
        match self {
            In::Null => std::process::Stdio::null(),
            In::Inherit => std::process::Stdio::inherit(),
            In::Bytes(_) => std::process::Stdio::piped(),
        }
    }
}

impl From<&[u8]> for In {
    fn from(value: &[u8]) -> Self {
        Self::Bytes(value.to_vec())
    }
}

impl From<&str> for In {
    fn from(value: &str) -> Self {
        value.as_bytes().into()
    }
}

impl From<&String> for In {
    fn from(value: &String) -> Self {
        value.as_bytes().into()
    }
}

impl From<String> for In {
    fn from(value: String) -> Self {
        Self::Bytes(value.into())
    }
}

#[derive(Debug, Clone, Default)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
struct VarsData<S: CmdString> {
    /// Indicates that all other environment variables shall be discarded.
    is_clean: bool,
    /// The values of the variables.
    values: HashMap<S, Option<S>>,
}

/// A set of environment variables.
#[derive(Debug, Clone, Default)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct Vars<S: CmdString = OsString>(Arc<VarsData<S>>);

impl<S: CmdString> Vars<S> {
    /// Constructs an empty set of environment variables.
    pub fn new() -> Self {
        Self(Default::default())
    }

    /// Indicates that all other environment variables shall be discarded.
    pub fn is_clean(&self) -> bool {
        self.0.is_clean
    }

    /// The values of the environment variables.
    pub fn values(&self) -> impl Iterator<Item = (&S::Str, Option<&S::Str>)> {
        self.0
            .values
            .iter()
            .map(|(k, v)| (k.as_ref(), v.as_ref().map(AsRef::as_ref)))
    }

    fn data_mut(&mut self) -> &mut VarsData<S> {
        Arc::make_mut(&mut self.0)
    }

    /// Sets the value of an environment variable.
    pub fn set<N: AsRef<S::Str>, V: AsRef<S::Str>>(&mut self, name: N, value: V) -> &mut Self {
        self.data_mut()
            .values
            .insert(name.as_ref().to_owned(), Some(value.as_ref().to_owned()));
        self
    }

    /// Discards the value of an environment variable.
    pub fn unset<N: AsRef<S::Str>>(&mut self, name: N) -> &mut Self {
        self.data_mut()
            .values
            .insert(name.as_ref().to_owned(), None);
        self
    }

    /// Inherits the environment variable from the parent process.
    pub fn inherit<N: AsRef<S::Str>>(&mut self, name: N) -> Result<&mut Self, std::env::VarError> {
        let name = name.as_ref();
        let os_name = AsRef::<OsStr>::as_ref(name);
        match std::env::var(os_name) {
            Ok(value) => {
                self.set(name, S::from_str(value.as_str()));
            }
            Err(std::env::VarError::NotPresent) => {
                self.unset(name);
            }
            Err(error) => {
                return Err(error);
            }
        }
        Ok(self)
    }

    /// Resets a variable.
    pub fn reset<N: AsRef<S::Str>>(&mut self, name: N) -> &mut Self {
        self.data_mut().values.remove(name.as_ref());
        self
    }
}

/// Private auxiliary macro. **Not part of the public API!**
#[macro_export]
#[doc(hidden)]
macro_rules! __private_populate_vars {
    ($env_vars:ident,) => {};
    ($env_vars:ident, $name:ident = $value:literal $(, $($vars:tt)*)?) => {
        $env_vars.set(stringify!($name), format!($value));
        $crate::__private_populate_vars!($env_vars, $($($vars)*)*);
    };
    ($env_vars:ident, $name:ident = $value:expr $(, $($vars:tt)*)?) => {
        $env_vars.set(stringify!($name), $value);
        $crate::__private_populate_vars!($env_vars, $($($vars)*)*);
    };
    ($env_vars:ident, $name:literal = $value:literal $(, $($vars:tt)*)?) => {
        $env_vars.set(format!($name), format!($value));
        $crate::__private_populate_vars!($env_vars, $($($vars)*)*);
    };
    ($env_vars:ident, $name:literal = $value:expr $(, $($vars:tt)*)?) => {
        $env_vars.set(format!($name), $value);
        $crate::__private_populate_vars!($env_vars, $($($vars)*)*);
    };
}

/// Convenience macro for constructing sets of variables.
///
/// ```rust
/// # use xscript::{vars_os as vars};
/// vars! {
///     RUSTDOCFLAGS = "--cfg docsrs --cfg xscript_unstable",
///     RUSTFLAGS = "--cfg xscript_unstable",
/// };
/// ```
#[macro_export]
macro_rules! vars {
    ($($vars:tt)*) => {{
        #[allow(unused_mut)]
        let mut env_vars = $crate::Vars::new();
        $crate::__private_populate_vars!(env_vars, $($vars)*);
        env_vars
    }};
}

/// Constructs environment variables using [`OsString`] as string type.
#[macro_export]
macro_rules! vars_os {
    ($($vars:tt)*) => {{
        let vars: $crate::Vars<::std::ffi::OsString> = $crate::vars!($($vars)*);
        vars
    }};
}

/// Constructs environment variables using [`String`] as string type.
#[macro_export]
macro_rules! vars_str {
    ($($vars:tt)*) => {{
        let vars: $crate::Vars<::std::string::String> = $crate::vars!($($vars)*);
        vars
    }};
}

/// Output produced when running a command.
#[derive(Debug, Clone, Default)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[non_exhaustive]
pub struct RunOutput {
    /// The exit code, if any.
    pub code: Option<i32>,
    /// The `stdout` output, if captured.
    pub stdout: Option<Vec<u8>>,
    /// The `stderr` output, if captured.
    pub stderr: Option<Vec<u8>>,
}

impl RunOutput {
    /// Constructs a new [`RunOutput`].
    pub fn new() -> Self {
        Self::default()
    }

    /// Sets the exit code of the command.
    pub fn with_code(mut self, code: i32) -> Self {
        self.code = Some(code);
        self
    }

    /// Sets the `stdout` output of the command.
    pub fn with_stdout(mut self, stdout: Vec<u8>) -> Self {
        self.stdout = Some(stdout);
        self
    }

    /// Sets the `stderr` output of the command.
    pub fn with_stderr(mut self, stderr: Vec<u8>) -> Self {
        self.stderr = Some(stderr);
        self
    }

    /// Tries to transform the `stdout` output to a string.
    fn try_into_stdout_str(self) -> Result<String, RunErrorKind> {
        self.stdout
            .ok_or_else(|| "no `stdout` output found".into())
            .and_then(|stdout| {
                String::from_utf8(stdout).map_err(|_| "`stdout` output is not valid UTF-8".into())
            })
            .map(|mut stdout| {
                while stdout.ends_with(|c: char| c.is_whitespace()) {
                    stdout.pop();
                }
                stdout
            })
    }
}

/// Error running a command.
#[derive(Debug)]
pub struct RunError<S: CmdString> {
    /// The command that failed.
    cmd: Cmd<S>,
    /// The kind of error.
    kind: RunErrorKind,
}

impl<S: CmdString> RunError<S> {
    /// Creates a new [`RunError`].
    pub fn new(cmd: Cmd<S>, kind: RunErrorKind) -> Self {
        Self { cmd, kind }
    }

    /// Transforms a [`RunErrorKind`] of a closure to [`RunError`].
    pub fn catch<F, U>(cmd: &Cmd<S>, func: F) -> RunResult<U, S>
    where
        F: FnOnce() -> Result<U, RunErrorKind>,
    {
        func().map_err(|kind| RunError::new(cmd.clone(), kind))
    }

    /// Transforms a [`RunErrorKind`] of a closure to [`RunError`].
    #[cfg(feature = "async")]
    #[cfg_attr(docsrs, doc(cfg(feature = "async")))]
    pub async fn catch_async<F, U, Fut>(cmd: &Cmd<S>, func: F) -> RunResult<U, S>
    where
        Fut: std::future::Future<Output = Result<U, RunErrorKind>>,
        F: FnOnce() -> Fut,
    {
        func()
            .await
            .map_err(|kind| RunError::new(cmd.clone(), kind))
    }
}

impl<S: CmdString> std::error::Error for RunError<S> {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match &self.kind {
            RunErrorKind::Failed { .. } => None,
            RunErrorKind::Io(error) => Some(error),
            RunErrorKind::Other(error) => Some(error.as_ref()),
            RunErrorKind::Custom(_) => None,
        }
    }
}

impl<S: CmdString> Display for RunError<S> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("error running command `{}`: ", self.cmd))?;
        match &self.kind {
            RunErrorKind::Failed(output) => {
                f.write_str("command failed with non-zero exit code")?;
                if let Some(code) = output.code {
                    f.write_char(' ')?;
                    Display::fmt(&code, f)?;
                }
                if let Some(stderr) = &output.stderr {
                    f.write_str("\n=== STDERR ===\n")?;
                    if let Ok(stderr) = std::str::from_utf8(stderr) {
                        f.write_str(stderr.trim())?;
                    } else {
                        f.write_str("<invalid utf-8>")?;
                    }
                }
            }
            RunErrorKind::Other(error) => {
                Display::fmt(&error, f)?;
            }
            RunErrorKind::Io(error) => {
                Display::fmt(&error, f)?;
            }
            RunErrorKind::Custom(message) => {
                Display::fmt(&message, f)?;
            }
        }
        Ok(())
    }
}

/// The result of running a command.
pub type RunResult<T, S> = Result<T, RunError<S>>;

/// Error while running a command.
#[derive(Debug)]
pub enum RunErrorKind {
    /// The command failed with a non-zero exit code.
    Failed(RunOutput),
    /// There was an [`io::Error`].
    Io(io::Error),
    /// A custom error message.
    Custom(String),
    /// The was some other error.
    Other(Box<dyn 'static + Sync + Send + std::error::Error>),
}

impl RunErrorKind {
    /// Constructs a [`RunErrorKind`] from some error.
    pub fn other<E: 'static + Sync + Send + std::error::Error>(error: E) -> Self {
        Self::Other(Box::new(error))
    }
}

impl From<RunOutput> for RunErrorKind {
    fn from(value: RunOutput) -> Self {
        Self::Failed(value)
    }
}

impl From<io::Error> for RunErrorKind {
    fn from(value: std::io::Error) -> Self {
        Self::Io(value)
    }
}

impl From<&str> for RunErrorKind {
    fn from(value: &str) -> Self {
        Self::Custom(value.to_owned())
    }
}

impl From<String> for RunErrorKind {
    fn from(value: String) -> Self {
        Self::Custom(value)
    }
}

/// Runs a command in a given environment (see [`Run::run`]).
#[macro_export]
macro_rules! run {
    ($env:expr, [$($cmd_args:tt)*] $($cmd_methods:tt)*) => {
        $env.run($crate::cmd!($($cmd_args)*)$($cmd_methods)*)
    };
    ([$($cmd_args:tt)*] $($cmd_methods:tt)*) => {
        $crate::ParentEnv.run($crate::cmd!($($cmd_args)*)$($cmd_methods)*)
    };
}

/// Runs a command in a given environment reading `stdout` as a string (see
/// [`Run::read_str`]).
#[macro_export]
macro_rules! read_str {
    ($env:expr, [$($cmd_args:tt)*] $($cmd_methods:tt)*) => {
        $env.read_str($crate::cmd!($($cmd_args)*)$($cmd_methods)*)
    };
    ([$($cmd_args:tt)*] $($cmd_methods:tt)*) => {
        $crate::ParentEnv.read_str($crate::cmd!($($cmd_args)*)$($cmd_methods)*)
    };
}

/// Runs a command in a given environment reading `stdout` as bytes (see
/// [`Run::read_bytes`])).
#[macro_export]
macro_rules! read_bytes {
    ($env:expr, [$($cmd_args:tt)*] $($cmd_methods:tt)*) => {
        $env.read_bytes($crate::cmd!($($cmd_args)*)$($cmd_methods)*)
    };
    ([$($cmd_args:tt)*] $($cmd_methods:tt)*) => {
        $crate::ParentEnv.read_bytes($crate::cmd!($($cmd_args)*)$($cmd_methods)*)
    };
}

/// Shared inner data of an execution environment.
#[derive(Debug, Clone)]
struct EnvInner {
    /// The working directory of the environment.
    cwd: PathBuf,
    /// The environment variables of the environment, if any.
    vars: Vars<OsString>,
    /// The default input provided to commands, if any.
    default_stdin: In,
    /// Indicates what to do with the `stdout` output by default.
    default_stdout: Out,
    /// Indicates what to do with the `stderr` output by default.
    default_stderr: Out,
    /// Replay any captured `stdout` output.
    replay_stdout: bool,
    /// Replay any captured `stderr` output.
    replay_stderr: bool,
    /// Echo commands before they are executed.
    echo_commands: bool,
}

impl EnvInner {
    fn new(cwd: PathBuf) -> Self {
        Self {
            cwd,
            vars: Vars::new(),
            default_stdin: In::Null,
            default_stdout: Out::Capture,
            default_stderr: Out::Capture,
            replay_stdout: false,
            replay_stderr: true,
            echo_commands: false,
        }
    }
}

/// Execution environment of the parent process.
pub struct ParentEnv;

impl Run<OsString> for ParentEnv {
    fn run(&self, cmd: Cmd<OsString>) -> Result<RunOutput, RunError<OsString>> {
        // TODO: This is inefficient, we should factor out the actual launch code.
        let env = RunError::catch(&cmd, || LocalEnv::current_dir().map_err(RunErrorKind::from))?;
        Run::run(&env, cmd)
    }
}

/// A local execution environment.
#[derive(Debug, Clone)]
pub struct LocalEnv(Arc<EnvInner>);

impl LocalEnv {
    /// Creates an execution environment with the given working directory.
    pub fn new<P: AsRef<Path>>(cwd: P) -> Self {
        Self(Arc::new(EnvInner::new(cwd.as_ref().to_path_buf())))
    }

    /// Creates an execution environment with the current working directory.
    pub fn current_dir() -> Result<Self, io::Error> {
        Ok(Self::new(std::env::current_dir()?))
    }

    fn inner_mut(&mut self) -> &mut EnvInner {
        Arc::make_mut(&mut self.0)
    }

    /// The working directory of the environment.
    pub fn cwd(&self) -> &Path {
        &self.0.cwd
    }

    /// Sets the working directory of the environment.
    pub fn set_cwd<P: AsRef<Path>>(&mut self, cwd: P) -> &mut Self {
        self.inner_mut().cwd = cwd.as_ref().to_path_buf();
        self
    }

    /// Sets the working directory of the environment.
    pub fn with_cwd<P: AsRef<Path>>(mut self, cwd: P) -> Self {
        self.set_cwd(cwd);
        self
    }

    /// The environment variables of the environment.
    pub fn vars(&self) -> &Vars {
        &self.0.vars
    }

    /// Sets the environment variables of the environment.
    pub fn set_vars(&mut self, vars: Vars) -> &mut Self {
        self.inner_mut().vars = vars;
        self
    }

    /// Sets the environment variables of the environment.
    pub fn with_vars(mut self, vars: Vars) -> Self {
        self.set_vars(vars);
        self
    }

    /// Sets an environment variable.
    pub fn set_var<N: AsRef<OsStr>, V: AsRef<OsStr>>(&mut self, name: N, value: V) -> &mut Self {
        self.inner_mut().vars.set(name, value);
        self
    }

    /// Sets an environment variable.
    pub fn with_var<N: AsRef<OsStr>, V: AsRef<OsStr>>(mut self, name: N, value: V) -> Self {
        self.set_var(name, value);
        self
    }

    /// The default `stdin` input to provide to commands.
    pub fn default_stdin(&self) -> &In {
        &self.0.default_stdin
    }

    /// Sets the default `stdin` input to provide to commands.
    pub fn with_default_stdin(mut self, stdin: In) -> Self {
        self.inner_mut().default_stdin = stdin;
        self
    }

    /// Indicates what to do with the `stdout` output of commands by default.
    pub fn default_stdout(&self) -> &Out {
        &self.0.default_stdout
    }

    /// Sets what to do with the `stdout` output of commands by default.
    pub fn with_default_stdout(mut self, stdout: Out) -> Self {
        self.inner_mut().default_stdout = stdout;
        self
    }

    /// Indicates what to do with the `stderr` output of commands by default.
    pub fn default_stderr(&self) -> &Out {
        &self.0.default_stderr
    }

    // Sets what to do with the `stderr` output of commands by default.
    pub fn with_default_stderr(mut self, stderr: Out) -> Self {
        self.inner_mut().default_stderr = stderr;
        self
    }

    /// Enables the echoing of commands.
    pub fn with_echo(mut self) -> Self {
        self.inner_mut().echo_commands = true;
        self
    }

    /// Disables the echoing of commands.
    pub fn without_echo(mut self) -> Self {
        self.inner_mut().echo_commands = false;
        self
    }

    /// Changes the working directory of the environment.
    pub fn change_dir<P: AsRef<Path>>(&mut self, path: P) -> Result<&mut Self, io::Error> {
        Ok(self.set_cwd(self.resolve_path(path).canonicalize()?))
    }

    /// Resolves a path relative to the working directory of the environment.
    pub fn resolve_path<P: AsRef<Path>>(&self, path: P) -> PathBuf {
        self.0.cwd.join(path.as_ref())
    }

    fn resolve_prog<'p>(&self, prog: &'p OsStr) -> Cow<'p, Path> {
        if prog.to_string_lossy().contains(std::path::is_separator) {
            Cow::Owned(self.resolve_path(prog))
        } else {
            Cow::Borrowed(Path::new(prog))
        }
    }

    fn echo_cmd(&self, cmd: &Cmd<OsString>) {
        if self.0.echo_commands {
            eprintln!("+ {cmd}");
        }
    }

    fn command(&self, cmd: &Cmd<OsString>) -> Command {
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
        command
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

/// Trait for running commands in an execution environment.
pub trait Run<S: CmdString> {
    /// Runs a command returning its output.
    fn run(&self, cmd: Cmd<S>) -> Result<RunOutput, RunError<S>>;

    /// Runs a command returning its `stdout` output as a string.
    fn read_str(&self, cmd: Cmd<S>) -> Result<String, RunError<S>> {
        let cmd = cmd.with_stdout(Out::Capture);
        self.run(cmd.clone())
            .and_then(|output| RunError::catch(&cmd, || output.try_into_stdout_str()))
    }

    /// Runs a command returning its `stderr` output as a string.
    fn read_bytes(&self, cmd: Cmd<S>) -> Result<Vec<u8>, RunError<S>> {
        let cmd = cmd.with_stdout(Out::Capture);
        self.run(cmd).map(|output| output.stdout.unwrap())
    }
}

impl Run<OsString> for LocalEnv {
    fn run(&self, cmd: Cmd<OsString>) -> Result<RunOutput, RunError<OsString>> {
        RunError::catch(&cmd, || {
            use io::Write;

            let cmd = &cmd;

            let mut command = self.command(cmd);
            self.echo_cmd(cmd);
            let mut child = command.spawn()?;
            let capture_stdout = child.stdout.is_some();
            let capture_stderr = child.stderr.is_some();
            let child_output = std::thread::scope(|scope| {
                if let Some(mut child_stdin) = child.stdin.take() {
                    scope.spawn(move || {
                        if let Some(In::Bytes(stdin)) = cmd.stdin() {
                            let _ = child_stdin.write_all(stdin);
                            let _ = child_stdin.flush();
                        }
                    });
                }
                child.wait_with_output()
            })?;
            if self.0.replay_stdout {
                let _ = io::stdout().write_all(&child_output.stdout);
            }
            if self.0.replay_stderr {
                let _ = io::stderr().write_all(&child_output.stderr);
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
    }
}

#[cfg(feature = "async")]
type BoxedFuture<'fut, T> = std::pin::Pin<Box<dyn 'fut + std::future::Future<Output = T>>>;

/// Trait for running commands asynchronously in an execution environment.
#[cfg_attr(docsrs, doc(cfg(feature = "async")))]
#[cfg(feature = "async")]
pub trait RunAsync<S: CmdString> {
    fn run(&self, cmd: Cmd<S>) -> BoxedFuture<RunResult<RunOutput, S>>;

    fn read_str(&self, cmd: Cmd<S>) -> BoxedFuture<RunResult<String, S>> {
        // Force capture the output.
        let cmd = cmd.with_stdout(Out::Capture);
        Box::pin(async move {
            self.run(cmd.clone())
                .await
                .and_then(|output| RunError::catch(&cmd, || output.try_into_stdout_str()))
        })
    }

    fn read_bytes(&self, cmd: Cmd<S>) -> BoxedFuture<Result<Vec<u8>, RunError<S>>> {
        let cmd = cmd.with_stdout(Out::Capture);
        Box::pin(async move { self.run(cmd).await.map(|output| output.stdout.unwrap()) })
    }
}

#[cfg(test)]
mod tests {
    use std::error::Error;

    use crate::{write_escaped, Run};

    #[test]
    fn test_write_escaped() {
        fn escape(string: &str) -> String {
            let mut buf = String::new();
            write_escaped(&mut buf, string).unwrap();
            buf
        }
        assert_eq!(escape("xyz"), "xyz");
        assert_eq!(escape("xyz abc"), "\"xyz abc\"");
        assert_eq!(escape("x\"yz\""), "x\\\"yz\\\"");
        assert_eq!(escape("\\x"), "\\\\x");
    }

    #[test]
    #[cfg(target_family = "unix")]
    fn test_io() -> Result<(), Box<dyn Error>> {
        use crate::LocalEnv;

        let env = LocalEnv::current_dir()?;
        assert!(read_str!(env, ["cat"])?.is_empty());
        assert_eq!(
            read_str!(env, ["cat"].with_stdin("Hello World!"))?,
            "Hello World!"
        );
        Ok(())
    }
}
