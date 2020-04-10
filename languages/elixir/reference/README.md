# Elixir reference

## Concepts

Below are the concepts that have been identified in Elixir for use in Exercism.

### Functional

- [Anonymous functions](../../../reference/concepts/anonymous_functions.md)
- [Higher-order functions](../../../reference/concepts/higher_order_functions.md)
- [Immutability](../../../reference/concepts/immutability.md)
  - Variable Scoping
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
    - format
    - clean
    - test
- Naming conventions
- Eager Computation
- Lazy Computation
- Guards
- Compiling
- Scripts (\*.exs) vs Code (\*.ex)
- Regex
- TODO: more

### Modules

- Naming
- Attributes
  - Redefining
- Alias
- Require
- Import
- Use
- Dynamic
- TODO: more

### Functions
- Private functions
- Named functions
  - Multi-line syntax
  - One-line syntax
- Naming conventions
  - functions prefixed with `is_`
  - functions ending in `?`
  - functions ending in `!`
  - functions prefixed with `do_`
- Argument positioning conventions
- [Default arguments](../../../reference/concepts/default_arguments.md)
- Capture Syntax
- Guards and defguard
- Parameters prefixed with `_`
- Local variables
- Implicit return
- Expression results
- Recursion
- Multi-clause functions

### General

- [Arithmetic](../../../reference/concepts/arithmetic.md)
- Basic Operators
- Logical Operators
- Comparison Operators
  - Type Comparison Hierarchy
- Control Structures
- Literal Forms of data structures (lists, keyword lists, maps)
- Bitwise functions and operators
- Case vs Cond vs Multiple function clauses
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
- Ok/Error tuples
  - {:ok, result}, {:error, reason}

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
| `booleans` | Introduction to the boolean type and strict boolean operators -- and/2, or/2, not/1 |
