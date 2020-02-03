# Elixir reference

## Concepts

Below are the concepts that have been identified in Elixir for use in Exercism.

### Functional

- [Anonymous functions](../../../reference/concepts/anonymous_functions.md)
- [Higher-order functions](../../../reference/concepts/higher_order_functions.md)
- [Immutability](../../../reference/concepts/immutability.md)
- [Pattern matching](../../../reference/concepts/pattern_matching.md)
- [Pipelines](../../../reference/concepts/pipelines.md)
- [Recursion](../../../reference/concepts/recursion.md)
- Tail Call Optimization

### Platform-specific

- BEAM VM
- Processes
- Agent Concurrency Model
- Erlang Interoperation
- String vs Charlist
  - UTF8/16/32 encoding
  - byte_size vs string length
- Interactive Console (REPL)
- elixir mix
  - directory structure
  - mix tasks
- Naming conventions
- Eager Computation
- Lazy Computation
- Guards
- Compiling
- Scripts (*.exs) vs Code (*.ex)
- TODO: more

### Modules

- Naming
- Attributes
- Alias
- Require
- Import
- Use
- TODO: more

### Functions

- Named functions
  - Multi-line syntax
  - One-line syntax
- Naming conventions
  - functions prefixed with `is_`
  - functions ending in `?`
  - functions ending in `!'
- Arugment positioning conventions
- [Default arguments](../../../reference/concepts/default_arguments.md)
- Capture Syntax

### General

- [Arithmetic](../../../reference/concepts/arithmetic.md)
- Basic Operators
- Logical Operators
- Comparison Operators
  - Type Comparison Hierarchy
- Control Structures
- Literal Forms of data structures (lists, keyword lists, maps)
- TODO: more

### Basic Types

- [Atom](../../../reference/types/symbol.md)
- Numbers
  - Integer
    - Binary, Octal, Hex forms
    - Codepoints
      - `?` operator
  - Float
    - Scientific Notation
- Binary
  - String
    - String Interpolation
  - Special Forms
- List
  - Notation `[head | tail]` syntax
  - Charlist
  - iodata
  - chardata
  - List Comprehensions
    - Generators
    - Filters
    - Into
- Tuple
- Function
- Reference
- PID

### Complex Types

- Keyword Lists
  - Role in function option gathering
- Maps
- Structs
- Ranges
- Streams
- Ports

### Protocols

- Protocols as Polymorphism
- Protocols on data types
- Protocols on structs

### Behaviours

- TODO: more

### Sigils

- TODO: more

### Error Handling

- Let it crash
- Try, Catch, Rescue

### IO and the Filesystem

- TODO: more

### Typespecs

- TODO: more

### Metaprogramming

- Abstract Syntax Tree
- Macro
- TODO: more

## Concept interpretation

TODO: Flesh out how the above general concepts apply to concept exercises in the Elixir track.

The concept exercises use the following concepts:

| concept | interpretation |
| --- | --- |
| `default-arguments` | Extract Elixir specifics from [official guide](https://elixir-lang.org/getting-started/modules-and-functions.html#default-arguments). |
