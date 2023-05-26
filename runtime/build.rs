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

fn cpp_flags(compiler: &cc::Tool) -> &'static [&'static str] {
    if !compiler.is_like_msvc() {
        static NON_MSVC_FLAGS: &[&str] = &[
            "-fno-rtti",
            "-fno-exceptions"
        ];
        NON_MSVC_FLAGS
    } else {
        static MSVC_FLAGS: &[&str] = &[
            "/EHsc", // C++ exceptions only, only in C++.
            "/GR-",  // Disable RTTI.
        ];
        MSVC_FLAGS
    }
}

fn main() {
    let target = env::var("TARGET").unwrap();
    let host = env::var("HOST").unwrap();
    let out_dir = env::var("OUT_DIR").unwrap();
    println!("cargo:warning=TARGET is {:?}", target);
    println!("cargo:warning=HOST is {:?}", host);
    println!("cargo:warning=OUT_DIR is {:?}", out_dir);
    let mut cfg = cc::Build::new();

    let compiler = cfg.get_compiler();
    for flag in cpp_flags(&compiler) {
        cfg.flag(flag);
    }

    let include_dirs = vec![Path::new("std"), Path::new("wrapper")];
    cfg.includes(include_dirs);    

    rerun_if_changed_anything_in_dir(Path::new("std"));
    cfg.file("std/stdcppshim.cpp")
        .file("std/io.cpp");

    rerun_if_changed_anything_in_dir(Path::new("wrapper"));
    cfg.file("wrapper/Allocator.cpp")
        .file("wrapper/Int.cpp")
        .file("wrapper/Float.cpp")
        .file("wrapper/Array.cpp")
        .file("wrapper/Tuple.cpp")
        .file("wrapper/Unit.cpp")
        .file("wrapper/Entry.cpp")
        .cpp(true)
        .cpp_link_stdlib(None) // cross compile, handle this below
        .compile("libsergeruntime_s.a");



    // Copy gernated static runtime library to runtime folder.
    // cp ../target/.../deps/.../out/libsergeruntime_s.a ../libsergeruntime_s.a
    let mut runtime_library_path = PathBuf::new();
    runtime_library_path.push(out_dir);
    runtime_library_path.push("libsergeruntime_s.a");
    let mut output_path = PathBuf::new();
    output_path.push(env::current_dir().unwrap().parent().unwrap());
    output_path.push("libsergeruntime_s.a");

    fs::copy(runtime_library_path, output_path).unwrap();

}