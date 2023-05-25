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
    let include_dirs = vec![Path::new("std"), Path::new("wrapper")];

    rerun_if_changed_anything_in_dir(Path::new("std"));
    cfg.file("std/stdcppshim.cpp")
        .file("std/io.cpp")
        .includes(&include_dirs);

    rerun_if_changed_anything_in_dir(Path::new("wrapper"));
    cfg.file("wrapper/Allocator.cpp")
        .file("wrapper/Int.cpp")
        .file("wrapper/Float.cpp")
        .file("wrapper/Array.cpp")
        .includes(&include_dirs)
        .cpp(true)
        .cpp_link_stdlib(None) // cross compile, handle this below
        .compile("libsergeruntime_s.a");

}