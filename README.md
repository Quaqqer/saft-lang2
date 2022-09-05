# saft-lang

Saft is a general purpose programming language, developed by me for fun.

## Goals

The goals that the language have are the following:

- Compiled - The language should be compiled to machine code.

- Type checked - The language shall be type checked.

- Extensible - The language should be extensible. No feature of the language
  should be hardcoded, but rather extensible by the programmer. The standard
  library, for instance, should be required. The user should be able to provide
  their own standard library with alternative implementations.

- Minimal - The language should be minimal. If a feature can be implemented with
  a combination of other features, that should be encouraged. Booleans for
  instance is not a type that should be defined internally, but rather defined
  in the standard library as an enum.

## Future goals

- Interpretor and repl
- Language server
- Tree-sitter parser
- Linter
- Build tool
