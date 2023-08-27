# CEL Parser

This module implements a LALRPOP parser for the [Common Expression Language](https://github.com/google/cel-spec).

## Usage:

```rust
use cel_parser::parse;

pub fn main() {
    let expr = parse("1 + 1").unwrap();
    println!("{:?}", expr);
}
```
