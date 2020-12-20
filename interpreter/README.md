# CEL Interpreter

This module implements a simple interpreter for the [Common Expression Language](https://github.com/google/cel-spec).

## Usage:

```rust
use cel_interpreter::Program;
use cel_interpreter::context::Context;

pub fn main() {
    let program = Program::compile("1 + 1 && [1,2,3].size() == 3").unwrap();
    let ctx = Context::default();
    println!("{:?}", program.execute(&ctx));
}
```
