use std::env;
use std::ffi;
use std::path::{Path, PathBuf};
use std::process::{Command, Stdio};


fn rerun_if_changed_anything_in_dir(dir: &Path) {
    // collect files except .git
    let mut stack = dir
        .read_dir()
        .unwrap()
        .map(|e| e.unwrap())
        .filter(|e| &*e.file_name() != ".git")
        .collect::<Vec<_>>();
    // recursively check subdirectories.
    while let Some(entry) = stack.pop() {
        let path = entry.path();
        if entry.file_type().unwrap().is_dir() {
            stack.extend(path.read_dir().unwrap().map(|e| e.unwrap()))
        } else {
            println!("cargo:rerun-if-changed={}", path.display());
        }
    }
}

fn output(cmd: &mut Command) -> String {
    let output = match cmd.stderr(Stdio::inherit()).output() {
        Ok(status) => status,
        Err(e) => {
            println!("\n\nFailed to execute command: {cmd:?}\nerror: {e}\n\n");
            std::process::exit(1);
        }
    };
    if !output.status.success() {
        panic!(
            "command did not execute successfully: {:?}\n\
            expected success, got: {}",
            cmd, output.status
        );
    }
    String::from_utf8(output.stdout).unwrap()
}


fn main() {

    let target = env::var("TARGET").expect("TARGET was not set");
    let host = env::var("HOST").expect("HOST was not set");

    let mut cfg = cc::Build::new();

    rerun_if_changed_anything_in_dir(Path::new("wrapper"));
    cfg.file("wrapper/GCObject.cpp")
        .target(&target)
        .host(&host)
        .cpp(true)
        .cpp_link_stdlib(None) // cross compile, handle this below
        .compile("runtime-wrapper");

    let stdcppname = if target.contains("darwin")
        || target.contains("windows-gnullvm")
    {
        "c++"
    } else {
        "stdc++"
    };

    // C++ runtime library
    // as our runtime is written by C++, it currently rely on C++ runtime
    if !target.contains("msvc") {
        println!("cargo:rustc-link-lib={stdcppname}");
        // TODO: cross compile to riscv/arm or remove c++ runtime"
    }


    // Libstdc++ depends on pthread which Rust doesn't link on MinGW
    // since nothing else requires it.
    if target.ends_with("windows-gnu") {
        println!("cargo:rustc-static-link-lib=static:-bundle=pthread");
    }

}