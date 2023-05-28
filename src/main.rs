use ariadne::{sources, Color, Label, Report, ReportKind};
use chumsky::prelude::*;
// use std::{borrow::Borrow, collections::HashMap, hash::Hash};

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

use inkwell::{
    builder::Builder,
    context::Context,
    module::Module,
    targets::{
        CodeModel, FileType, InitializationConfig, RelocMode, Target, TargetMachine, TargetTriple,
    },
    OptimizationLevel,
};

use llvm_sys::{*, transforms::pass_builder::{LLVMRunPasses, LLVMCreatePassBuilderOptions, LLVMPassBuilderOptionsSetDebugLogging}};
use llvm_sys::core::*;
use llvm_sys::prelude::*;
use llvm_sys::target_machine::*;
use libc::*;

use backend::codegen::CodeGen;

use std::ffi::OsStr;
use std::path::{Path, PathBuf};
use std::ptr::null_mut;
use std::mem::MaybeUninit;

fn call_system_linker(input: &Path, output: &Path) -> Result<std::process::Output, String> {
    use std::process::Command;

    Command::new("clang")
        .args([
            "--target",
            "riscv64-unknown-linux-elf",
            "-march=rv64imfd",
            "-mabi=lp64d",
            "libsergeruntime_s.a",
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
        let opt_level = LLVMCodeGenOptLevel::LLVMCodeGenLevelDefault;
        let reloc_mode = LLVMRelocMode::LLVMRelocPIC;
        let code_model = LLVMCodeModel::LLVMCodeModelDefault;
        // set target machine
        let triple = "riscv64-unknown-linux-gnu";
        let target = null_mut::<LLVMTargetRef>();
        let mut error_string = MaybeUninit::uninit();
        let return_code = LLVMGetTargetFromTriple(triple.as_ptr() as *const i8, 
                                  target,
                                    error_string.as_mut_ptr());
        let cpu = "generic";
        let target_machine = LLVMCreateTargetMachine(*target,
                                            triple.as_ptr() as *const i8,
                                    cpu.as_ptr() as *const c_char,
                                "".as_ptr() as *const c_char,
                                    opt_level,
                                    reloc_mode,
                                    code_model);

        // run passes
        let pass_options = LLVMCreatePassBuilderOptions();
        LLVMPassBuilderOptionsSetDebugLogging(pass_options, 1);
   
        LLVMRunPasses(module, "default<O2>".as_ptr() as *const i8, target_machine, pass_options);
        // emit object fie
        let file_type = LLVMCodeGenFileType::LLVMObjectFile;
        let mut err_message = MaybeUninit::uninit();
        let return_code = LLVMTargetMachineEmitToFile(target_machine, 
                                                            module, 
                                                            "output.o".as_ptr() as *mut i8, 
                                                            file_type, 
                                                            err_message.as_mut_ptr());
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
            match TypedModule::create_from_ast(&ast) {
                Ok(typed_ast) => {
                    println!("type check passed");
                    // println!("{:#?}", typed_ast);
                    let mut mir = MIR::create_from_typed_ast(&typed_ast);
                    println!("{:#?}", mir);
                    let mut llvm_ir_codegen = CodeGen::new(
                        &mir.name_ctx,
                        &mir.ty_ctx
                    );
                    llvm_ir_codegen.create_module(&mir);
                    // emit object
                    emit_object(llvm_ir_codegen.module);
                    // call linker here

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
                .with_message(e.to_string())
                .with_label(
                    Label::new((filename.clone(), e.span().into_range()))
                        .with_message(e.reason().to_string())
                        .with_color(Color::Red),
                )
                .finish()
                .print(sources([(filename.clone(), src.clone())]))
                .unwrap()
        });

}
