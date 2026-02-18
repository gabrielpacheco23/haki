use crate::{ExecMode, env::Env, heap::Heap, run_source};

pub fn load_stdlib(lib_code: &str, env: &mut Env, heap: &mut Heap) {
    if let Err(e) = run_source(lib_code, env, ExecMode::Normal, heap, false) {
        eprintln!("Error loading standard library: {}", e);
    }
}
