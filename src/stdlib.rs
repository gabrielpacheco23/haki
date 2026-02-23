use crate::{ExecMode, compiler::CompilerState, env::Env, heap::Heap, run_source};

pub fn load_stdlib(lib_code: &str, env: &mut Env, heap: &mut Heap) {
    if let Err(e) = run_source(
        lib_code,
        env,
        ExecMode::Normal,
        heap,
        false,
        &mut CompilerState::new(),
        false,
    ) {
        eprintln!("Error loading standard library: {}", e);
    }
}
