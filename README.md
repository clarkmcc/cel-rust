# cel-rust

This repository contains several modules that implement a parser, interpreter and CLI for
the [Common Expression Language](https://github.com/google/cel-spec). This has been forked
from [orf/cel-rust](https://github.com/orf/cel-rust).

My goal is to bring the crate back up to speed with
the [CEL spec](https://github.com/google/cel-spec/blob/master/doc/intro.md) and then implement a few handy extensions
beyond what the spec defines.

Changes in this fork:

* The [extensions](#extensions) described below.
* Functions are fallible and return an execution error rather than panicking.
* Attribute accessing on maps which was not working under the original implementation.

## Usage

```rust
fn main() {
    let program = Program::compile("1 == 1").unwrap();
    let context = Context::default();
    let value = program.execute(&context).unwrap();
    assert_eq!(value, true.into());
}
```

## Extensions

### Set Operations

#### Merge Maps

You can merge two maps using the `+` operator. The right hand side map will overwrite any keys that exist in the left
map.

```cel
{'a': 1} + {'b': 2} == {'a': 1, 'b': 2}
```

#### Difference

Performs a set difference using the keys of the map. Values are ignored.

```cel
{'a': 1, 'b': 1} - {'b': 2} == {'a': 1}
```

You can also perform a set difference on arrays:

```cel
[1, 2, 3] - [2, 3] == [1]
```

### String Indexing

CEL allows you to index into arrays like

```cel
[1, 2, 3][0] == 1
```

This extension allows you to index into strings in the same way:

```cel
'hello'[0] == 'h'
```

## Crates

* [parser](./parser) - Implements a LALRPOP based parser for the language
* [interpreter](./interpreter) - Implements a simple interpreter for the language
* [cli](./cli) - A basic terminal interface to execute CEL expressions
