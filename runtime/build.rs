use std::env;
use std::ffi;
use std::fs;
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
    println!("cargo:warning=TARGET is {:?}", env::var("TARGET").unwrap());
    println!("cargo:warning=HOST is {:?}", env::var("HOST").unwrap());
    println!("cargo:warning=OUT_DIR is {:?}", env::var("OUT_DIR").unwrap());

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


    // Copy gernated static runtime library to runtime folder.
    // cp ../target/.../deps/.../out/libsergeruntime_s.a ../libsergeruntime_s.a
    let mut runtime_library_path = PathBuf::new();
    runtime_library_path.push(env::var("OUT_DIR").unwrap());
    runtime_library_path.push("libsergeruntime_s.a");
    let mut output_path = PathBuf::new();
    output_path.push(env::current_dir().unwrap().parent().unwrap());
    output_path.push("libsergeruntime_s.a");

    fs::copy(runtime_library_path, output_path).unwrap();

}