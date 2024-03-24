//! Methods for reading files and environment variables and for running shell commands

use std::{env, io, path, process};

use itertools::Itertools;

/// Reads a path relative to caller location from `var`
///
/// Returns `VarError` if the var does not exist.
///
/// # Panics
///
/// When the current working dir cannot be accessed.
pub(crate) fn read_relpath_from_env(var: &str) -> Result<path::PathBuf, env::VarError> {
    let path = env::var(var)?;
    Ok(env::current_dir()
        .expect("cannot access current working dir")
        .join(path))
}

/// Reads an absolute path from env
///
/// Returns `VarError` if the var does not exist.
pub(crate) fn read_abspath_from_env(var: &str) -> Result<path::PathBuf, env::VarError> {
    let path = env::var(var)?;
    Ok(path::PathBuf::from(path))
}

/// Returns a vector from `var`
///
/// Returns `VarError` if the var does not exist.
///
/// # Parameters:
///
/// `var` - The name of the environment variable to be read
/// `sep` - The separator for Vec elements
pub(crate) fn read_vec_from_env(var: &str, sep: char) -> Result<Vec<String>, env::VarError> {
    env::var(var).map(|s| {
        // TODO: we could query `keelhaul` at this point to verify that these are valid peripherals
        s.split(sep).map(ToOwned::to_owned).collect_vec()
    })
}

/// Execute shell command
pub(crate) fn run_cmd(cmd: &str, params: &[impl AsRef<str>]) -> io::Result<()> {
    let mut cmd = &mut process::Command::new(cmd);
    for param in params {
        cmd = cmd.arg(param.as_ref());
    }
    cmd.spawn()?;
    Ok(())
}
