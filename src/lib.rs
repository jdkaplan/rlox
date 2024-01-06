macro_rules! debug_log_gc {
    ($($arg:tt)*) => {{
        #[cfg(feature = "log_gc")]
        eprintln!($($arg)*);
    }};
}

mod chunk;
mod compiler;
mod gc;
mod heap;
mod object;
mod scanner;
mod table;
mod value;
mod vm;

pub use vm::{InterpretError, InterpretResult, Vm, VmOptions};

pub(crate) const U8_COUNT: usize = (u8::MAX as usize) + 1;
pub(crate) const FRAMES_MAX: usize = 64;
pub(crate) const STACK_MAX: usize = FRAMES_MAX * U8_COUNT;

#[cfg(test)]
mod tests {
    use std::path::{Path, PathBuf};

    use rstest::rstest;

    use super::*;

    struct RunResult {
        result: InterpretResult<()>,
        stdout: String,
        stderr: String,
    }

    fn run(source: &str) -> RunResult {
        let mut stdout = Vec::new();
        let mut stderr = Vec::new();

        let result = {
            let mut vm = Vm::new(VmOptions {
                stdout: Box::new(&mut stdout),
                stderr: Box::new(&mut stderr),
            });
            vm.interpret(source)
        };

        RunResult {
            result,
            stdout: String::from_utf8(stdout).unwrap(),
            stderr: String::from_utf8(stderr).unwrap(),
        }
    }

    fn snapshot_name(path: &Path, name: &str) -> String {
        let crate_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
        let relative = path.strip_prefix(crate_root).unwrap();
        format!("{}.{}", relative.to_string_lossy(), name)
    }

    #[rstest]
    fn test_programs(#[files("test_programs/*.lox")] path: PathBuf) {
        let source = std::fs::read_to_string(&path).unwrap();
        let run = run(&source);

        assert!(run.result.is_ok(), "{:?}", run.stderr);

        insta::with_settings!({
            input_file => &path,
            description => source,
            omit_expression => true,
        }, {
            insta::assert_snapshot!(snapshot_name(&path, "stdout"), run.stdout);
        })
    }

    #[rstest]
    fn test_compile_errors(#[files("test_programs/err_compile/*.lox")] path: PathBuf) {
        let source = std::fs::read_to_string(&path).unwrap();
        let run = run(&source);

        assert!(
            matches!(run.result, Err(InterpretError::Compile(_))),
            "{:?}",
            run.stderr
        );

        insta::with_settings!({
            input_file => &path,
            description => source,
            omit_expression => true,
        }, {
            insta::assert_snapshot!(snapshot_name(&path, "stderr"), run.stderr);
        })
    }

    #[rstest]
    fn test_runtime_errors(#[files("test_programs/err_runtime/*.lox")] path: PathBuf) {
        let source = std::fs::read_to_string(&path).unwrap();
        let run = run(&source);

        assert!(
            matches!(run.result, Err(InterpretError::Runtime(_))),
            "{:?}",
            run.stderr
        );

        insta::with_settings!({
            input_file => &path,
            description => source,
            omit_expression => true,
        }, {
            insta::assert_snapshot!(snapshot_name(&path, "stdout"), run.stdout);
            insta::assert_snapshot!(snapshot_name(&path, "stderr"), run.stderr);
        })
    }
}
