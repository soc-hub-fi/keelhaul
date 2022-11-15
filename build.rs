use std::env;
use std::process::Command;

fn parse() {
    let output = Command::new("python3.10")
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

fn generate() {
    let output = Command::new("python3.10")
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
    // Only parse when necessary.
    let do_parse = env::var("NO_PARSE").is_err();
    if do_parse {
        parse();
    }
    generate();
}
