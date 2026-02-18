use crate::{ExecMode, env::Env, run_source};

pub fn load_stdlib(lib_code: &str, env: &mut Env) {
    if let Err(e) = run_source(lib_code, env, ExecMode::Normal) {
        eprintln!("Error loading standard library: {}", e);
    }
}
