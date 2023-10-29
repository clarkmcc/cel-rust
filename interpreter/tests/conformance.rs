mod parse_utilities;

use cel_interpreter::{Context, Program};
use cucumber::{given, then, when, World};

// `World` is your shared, likely mutable state.
// Cucumber constructs it via `Default::default()` for each scenario.
#[derive(Debug, Default, World)]
pub struct CelWorld {
    expression: String,
    context: Context<'static>,
}

#[when(expr = "CEL expression \"{word}\" is evaluated")]
fn expression_evaluated_with_double_quotes(world: &mut CelWorld, expression: String) {
    world.expression = expression;
}

#[when(expr = "CEL expression '{word}\' is evaluated")]
fn expression_evaluated_with_single_quotes(world: &mut CelWorld, expression: String) {
    world.expression = expression;
}

// Given: bindings parameter "x" is IntType(source=123)
#[given(regex = "^bindings parameter \"([a-z]+)\" is (.*)$")]
fn bindings_parameter_is(world: &mut CelWorld, name: String, value: String) {
    println!("bindings_parameter_is: name={}, value={}", name, value);
    // world.context.add_variable(name, value);
}

#[then(regex = "value is (.*)")]
fn evaluation_result_is(world: &mut CelWorld, expected_result: String) {
    let program = Program::compile(&world.expression).unwrap();
    let result = program.execute(&world.context);
    let expect = cel_interpreter::gherkin::parse_value(&expected_result).unwrap();
    assert_eq!(expect, result.unwrap());
}

fn main() {
    futures::executor::block_on(CelWorld::run("tests/features"));
}
