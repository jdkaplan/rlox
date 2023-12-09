use std::env;
use std::ffi::CString;
use std::io;
use std::io::Write;
use std::process;

use rlox::{InterpretResult, Vm};

fn main() {
    // TODO: clap
    let args: Vec<String> = env::args().collect();
    match args.len() {
        1 => repl(),
        2 => run_file(&args[1]),
        _ => {
            eprintln!("Usage: {} [path]", args[0]);
            process::exit(64);
        }
    }
}

fn repl() {
    let mut vm = Vm::new();

    // TODO: readline or better
    let mut line = String::new();
    loop {
        print!("> ");
        io::stdout().flush().unwrap();

        line.clear();
        match io::stdin().read_line(&mut line) {
            Err(err) => {
                eprintln!("{}", err);
                break;
            }

            Ok(0) => break,

            Ok(_n) => {
                let Ok(source) = CString::new(line.clone()) else {
                    eprintln!("error: the line was not a valid C string");
                    continue;
                };

                _ = vm.interpret(source.as_ptr());
            }
        }
    }
}

fn run_file(path: &str) {
    let mut vm = Vm::new();

    let Ok(source) = std::fs::read_to_string(path) else {
        eprintln!("error: could not read file at {:?}", path);
        process::exit(74);
    };

    let Ok(source) = CString::new(source) else {
        eprintln!("error: the source code was not a valid C string");
        process::exit(74);
    };

    match vm.interpret(source.as_ptr()) {
        InterpretResult::InterpretOk => {}
        InterpretResult::InterpretCompileError => process::exit(65),
        InterpretResult::InterpretRuntimeError => process::exit(70),
    };
}
