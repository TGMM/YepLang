#![feature(lazy_cell)]
#![feature(let_chains)]
#![feature(path_file_prefix)]

use ariadne::{Color, Label, Report, ReportKind, Source};
use clap::Parser;
use compiler::{helpers::YepTarget, main_codegen::compile_yep};
use std::{fs, path::Path};
use yep_lang::compiler::{self, main_codegen::CompilerArgs};

const TARGET: &str = env!("TARGET");

/// Simple program to greet a person
#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
struct Args {
    /// Name of the file to compile
    #[arg(index = 1)]
    name: String,
    /// Directory to output the file to
    #[arg(long)]
    out: Option<String>,
    /// Compile but do not link
    #[arg(long)]
    skip_link: bool,
    /// Emit LLVM IR
    #[arg(long)]
    emit_llvm: bool,
    /// Emit assembly
    #[arg(long)]
    emit_assembly: bool,
    /// The target to compile to
    #[arg(long, default_value_t = TARGET.to_string())]
    target_triple: String,
    /// Signals to the compiler that the
    /// target environment doesn't have libc available
    #[arg(long)]
    no_std: bool,
}

fn main() -> Result<(), String> {
    let args = Args::parse();

    let input_path = Path::new(args.name.as_str());
    if !input_path.is_file() {
        return Err("The input path must point to a valid file".to_string());
    }

    let input_name_no_ext = input_path.with_extension("");
    let input_name = input_name_no_ext.file_name().unwrap();

    let input = fs::read_to_string(input_path).map_err(|err| err.to_string())?;
    let input_ref: &'static str = Box::leak(input.into_boxed_str());

    let target = YepTarget {
        target_triple: args.target_triple,
        nostd: args.no_std,
    };

    let compiler_args = CompilerArgs {
        skip_link: args.skip_link,
        emit_llvm: args.emit_llvm,
        emit_assembly: args.emit_assembly,
        skip_compile: false,
    };

    let (out_path, out_name): (String, String) = if let Some(arg_path_str) = args.out {
        let arg_path = Path::new(&arg_path_str);

        if arg_path.is_dir() {
            (
                arg_path.join(input_name).to_str().unwrap().to_string(),
                input_name.to_str().unwrap().to_string(),
            )
        } else {
            let arg_path_no_ext = arg_path.with_extension("");

            (
                arg_path_no_ext.to_str().unwrap().to_string(),
                arg_path_no_ext
                    .file_name()
                    .unwrap()
                    .to_str()
                    .unwrap()
                    .to_string(),
            )
        }
    } else {
        let curr_dir = std::env::current_dir().unwrap();
        let curr_dir_path = Path::new(&curr_dir);

        if curr_dir_path.is_dir() {
            (
                curr_dir_path.join(input_name).to_str().unwrap().to_string(),
                input_name.to_str().unwrap().to_string(),
            )
        } else {
            let curr_dir_path_no_ext = curr_dir_path.with_extension("");

            (
                curr_dir_path_no_ext.to_str().unwrap().to_string(),
                curr_dir_path
                    .with_extension("")
                    .to_str()
                    .unwrap()
                    .to_string(),
            )
        }
    };

    let res = compile_yep(input_ref, out_path, out_name, target, compiler_args);
    match res {
        Ok(_) => {}
        Err(e) => {
            let file_name = "test.yep";
            if let Some(span) = e.span {
                Report::build(ReportKind::Error, file_name, span.start)
                    .with_code(3)
                    .with_message(e.reason.clone())
                    .with_label(
                        Label::new((file_name, span.into_range()))
                            .with_message(e.reason)
                            .with_color(Color::Red),
                    )
                    .finish()
                    .eprint((file_name, Source::from(input_ref)))
                    .unwrap()
            } else {
                eprintln!("{}", e.reason);
            }
        }
    }

    Ok(())
}
