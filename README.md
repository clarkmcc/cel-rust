# cel-rust

This repository contains several modules that implement a parser, interpreter and CLI for the [Common Expression Language](https://github.com/google/cel-spec).

The aim is to create a fast, simple and readable interpreter for the language that can be used as an example of how to 
create other language runtimes. 

## Crates

* [parser](./parser) - Implements a LALRPOP based parser for the language
* [interpreter](./interpreter) - Implements a simple interpreter for the language
* [cli](./cli) - A basic terminal interface to execute CEL expressions
