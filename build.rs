use std::env;
use std::process::Command;

fn parse(path_python: String) {
    let output = Command::new(path_python)
        .arg("scripts/parse.py")
        .output()
        .expect("Failed to run command.");
    let stdout =
        String::from_utf8(output.stdout).expect("Failed to transform command output to UTF-8.");
    let stderr =
        String::from_utf8(output.stderr).expect("Failed to transform command output to UTF-8.");
    let code = output.status.code().expect("Command status code was None.");
    println!("cargo:warning=Status code: {code}.");
    println!("cargo:warning=Standard output:");
    for line in stdout.lines() {
        println!("cargo:warning={}", line);
    }
    println!("cargo:warning=Standard error:");
    for line in stderr.lines() {
        println!("cargo:warning={}", line);
    }
}

fn generate(path_python: String) {
    let output = Command::new(path_python)
        .arg("scripts/generate.py")
        .output()
        .expect("Failed to run command.");
    let stdout =
        String::from_utf8(output.stdout).expect("Failed to transform command output to UTF-8.");
    let stderr =
        String::from_utf8(output.stderr).expect("Failed to transform command output to UTF-8.");
    let code = output.status.code().expect("Command status code was None.");
    println!("cargo:warning=Status code: {code}.");
    println!("cargo:warning=Standard output:");
    for line in stdout.lines() {
        println!("cargo:warning={}", line);
    }
    println!("cargo:warning=Standard error:");
    for line in stderr.lines() {
        println!("cargo:warning={}", line);
    }
}

fn main() {
    println!("cargo:rerun-if-changed=data/tackle.svd");
    println!("cargo:rerun-if-changed=scripts/parse.py");
    println!("cargo:rerun-if-changed=scripts/generate.py");
    println!("cargo:rerun-if-env-changed=NO_PARSE");
    println!("cargo:rerun-if-env-changed=PATH_PYTHON");
    let path_python = env::var("PATH_PYTHON").expect("Missing environment variable: PATH_PYTHON");
    // Only parse when necessary.
    let do_parse = env::var("NO_PARSE").is_err();
    if do_parse {
        parse(path_python.clone());
    }
    generate(path_python);
}
