use crate::context::Context;
use crate::objects::ResolveResult;
use crate::Program;

/// Tests the provided script and returns the result. An optional context can be provided.
pub(crate) fn test_script(script: &str, ctx: Option<Context>) -> ResolveResult {
    let program = Program::compile(script).unwrap();
    program.execute(&ctx.unwrap_or(Context::default()))
}

// pub(crate) fn test_script_error(script: &str, ctx: Option<&Context>)
