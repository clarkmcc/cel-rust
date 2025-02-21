use crate::{Context, Program, ResolveResult};

pub(crate) fn assert_script(input: &(&str, &str)) {
    assert_eq!(test_script(input.1, None), Ok(true.into()), "{}", input.0);
}

/// Tests the provided script and returns the result. An optional context can be provided.
pub(crate) fn test_script(script: &str, ctx: Option<Context>) -> ResolveResult {
    let program = Program::compile(script).unwrap();
    program.execute(&ctx.unwrap_or_default())
}
