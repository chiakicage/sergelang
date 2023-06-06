use ariadne::{sources, Color, Label, Report, ReportKind};
use chumsky::prelude::*;
// use std::{borrow::Borrow, collections::HashMap, hash::Hash};
use crate::utils::error::{get_code, get_msg};
mod ast;
mod backend;
mod frontend;
mod midend;
mod utils;

use ast::*;
use frontend::lexer::lexer;
use frontend::parser::parser;
use midend::mir::MIR;
use midend::typed_ast::TypedModule;
use utils::error::Span;

use libc::*;
use llvm_sys::core::*;
use llvm_sys::prelude::*;
use llvm_sys::target::*;
use llvm_sys::target_machine::*;
use llvm_sys::transforms::pass_builder::*;

use backend::codegen::CodeGen;
use utils::to_c_str;

use std::ffi::{CString, OsStr};
use std::mem::MaybeUninit;
use std::path::{Path, PathBuf};
use std::ptr::null_mut;

fn call_system_linker(input: &Path, output: &Path) -> Result<std::process::Output, String> {
    use std::process::Command;

    Command::new("clang++")
        .args([
            "-target",
            "riscv64-unknown-linux-gnu",
            "-march=rv64imfd",
            "-mabi=lp64d",
            "--sysroot=/usr/riscv64-linux-gnu",
            "./runtime/libserge_runtime.a",
            "-O1",
            "-fuse-ld=lld",
        ])
        .arg(input)
        .arg("-o")
        .arg(output)
        .output()
        .map_err(|e| e.to_string())
}

fn emit_object(module: LLVMModuleRef) {
    unsafe {
        // initialization
        LLVM_InitializeAllTargetInfos();
        LLVM_InitializeAllTargets();
        LLVM_InitializeAllTargetMCs();
        LLVM_InitializeAllAsmParsers();
        LLVM_InitializeAllAsmPrinters();

        let opt_level = LLVMCodeGenOptLevel::LLVMCodeGenLevelDefault;
        let reloc_mode = LLVMRelocMode::LLVMRelocPIC;
        let code_model = LLVMCodeModel::LLVMCodeModelDefault;
        // set target machine
        let triple = LLVMCreateMessage(to_c_str("riscv64-unknown-linux-gnu").as_ptr() as *const _);
        LLVMSetTarget(module, triple);

        // create target.
        let mut target = std::ptr::null_mut();
        let mut error_string = MaybeUninit::uninit();
        let return_code = LLVMGetTargetFromTriple(triple, &mut target, error_string.as_mut_ptr());
        if return_code != 0 {
            puts(*error_string.as_ptr());
            exit(1);
        }

        // create target machine.
        let cpu = "generic";
        let target_machine = LLVMCreateTargetMachine(
            target,
            triple,
            to_c_str(cpu).as_ptr(),
            to_c_str("+a,+c,+f,+m,+d").as_ptr(),
            opt_level,
            reloc_mode,
            code_model,
        );

        // run passes
        let pass_options = LLVMCreatePassBuilderOptions();
        LLVMPassBuilderOptionsSetDebugLogging(pass_options, 1);

        LLVMRunPasses(
            module,
            to_c_str("default<O2>").as_ptr(),
            target_machine,
            pass_options,
        );
        let mut err_string = MaybeUninit::uninit();
        LLVMPrintModuleToFile(
            module,
            to_c_str("build/output_opt.ll").as_ptr(),
            err_string.as_mut_ptr(),
        );
        // emit object fie
        let file_type = LLVMCodeGenFileType::LLVMObjectFile;
        let mut err_message = MaybeUninit::uninit();
        let return_code = LLVMTargetMachineEmitToFile(
            target_machine,
            module,
            to_c_str("build/output.o").as_ptr() as *mut _,
            file_type,
            err_message.as_mut_ptr(),
        );
        if return_code != 0 {
            puts(*error_string.as_ptr());
            exit(1);
        }
    }
}

fn main() {
    let filename = std::env::args().nth(1).unwrap();
    let src = std::fs::read_to_string(&filename).unwrap();

    println!("{:#?}", src);
    let (tokens, errs) = lexer().parse(src.as_str()).into_output_errors();
    println!("{:?}", tokens);
    let parse_errs = if let Some(tokens) = &tokens {
        let (ast, parse_errs) = parser()
            .parse(
                tokens
                    .as_slice()
                    .spanned(Span::new(src.len(), src.len()))
                    .into(),
            )
            .into_output_errors();
        if let Some(ast) = ast {
            ast_walk(&ast);
            let mut errs = Vec::new();
            let mut output_file = PathBuf::from("build/output.o");
            match TypedModule::create_from_ast(&ast) {
                Ok(typed_ast) => {
                    println!("type check passed");
                    // println!("{:#?}", typed_ast);
                    let mut mir = MIR::create_from_typed_ast(&typed_ast);
                    println!("generate middle IR");
                    println!("{:#?}", mir);
                    // emit LLVM IR
                    let mut llvm_ir_codegen = CodeGen::new(&mir.name_ctx, &mir.ty_ctx);
                    llvm_ir_codegen.create_module(&mir);
                    // emit LLVM IR file
                    unsafe {
                        let mut err_string = MaybeUninit::uninit();
                        LLVMPrintModuleToFile(
                            llvm_ir_codegen.module,
                            to_c_str("build/output.ll").as_ptr(),
                            err_string.as_mut_ptr(),
                        );
                    }
                    println!("emit LLVM IR file done");
                    // emit object
                    emit_object(llvm_ir_codegen.module);
                    // call linker here
                    let handle_err = |err: String| -> Result<(), ()> {
                        println!("{} {:?}", "::", err.as_str());
                        Err(())
                    };
                    let handle_output = |out: std::process::Output| -> Result<(), ()> {
                        if out.status.success() {
                            println!("{} Try to link the object file", "::");
                        } else {
                            println!("{} Try to link the object file", "::");
                        }
                        let stdout = std::str::from_utf8(out.stdout.as_slice()).unwrap_or_default();
                        let stderr = std::str::from_utf8(out.stderr.as_slice()).unwrap_or_default();
                        if !stderr.is_empty() || !stdout.is_empty() {
                            println!("{} stderr: {} stdout: {}", "::", stderr, stdout);
                            return Err(());
                        }
                        Ok(())
                    };
                    if call_system_linker(
                        &output_file.with_extension("o"),
                        &output_file.with_extension(""),
                    )
                    .map_or_else(handle_err, handle_output)
                    .is_ok()
                    {
                        println!(
                            "{} Write binary file into {:?}",
                            "::",
                            output_file.with_extension("").as_os_str()
                        );
                    }
                }
                Err(err) => {
                    errs.push(err);
                }
            }
            errs.into_iter()
                .map(|e| e.map_token(|tok| tok.to_string()))
                .collect::<Vec<_>>()
        } else {
            parse_errs
                .into_iter()
                .map(|e| e.map_token(|tok| tok.to_string()))
                .collect::<Vec<_>>()
        }
    } else {
        Vec::new()
    };

    errs.into_iter()
        .map(|e| e.map_token(|c| c.to_string()))
        .chain(parse_errs.into_iter())
        .for_each(|e| {
            Report::build(ReportKind::Error, filename.clone(), e.span().start)
                .with_code(get_code(&e.to_string()))
                .with_message(get_msg(&e.to_string()))
                .with_label(
                    Label::new((filename.clone(), e.span().into_range()))
                        // .with_message(e.reason().to_string())
                        .with_message(get_msg(&e.to_string()))
                        .with_color(Color::Red),
                )
                .finish()
                .print(sources([(filename.clone(), src.clone())]))
                .unwrap()
        });
}
