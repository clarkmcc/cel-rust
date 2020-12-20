# CEL Parser

This module implements a LALRPOP parser for the [Common Expression Language](https://github.com/google/cel-spec).

## Usage:

```rust
use cel_parser::ast::Expression;
use cel_parser::parser::ExpressionParser;

pub fn main() {
    let ast = ExpressionParser::new().parse("1 + 1 && [1,2,3].size() == 3").unwrap();
    println!("{:?}", ast);
}
```
