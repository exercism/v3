# Elixir reference

## Concepts

Below are the concepts that have been identified in Elixir for use in Exercism.
The initial breakdown of these concepts, including the ordering, is based on the elixir-lang.org [Getting Started Guide](https://elixir-lang.org/getting-started/introduction.html).

### Useful information not tested by exercises
- `iex`
  - `h/0`, plus `h/1`, `i`, `v`, etc.
- `elixir script.exs` to execute a script
- `IO.puts/1` and `IO.inspect/2`

### elixir-lang.org Getting Started Guide concept extraction

- [Arithmetic](../../../reference/concepts/arithmetic.md)
  - `+`, `-`, `*`, and `/` operators
  - `/` always returns a [`Float`](../../../reference/types/floating_point_number.md)
  - `div` and `rem` for integer division and modulo
  - Binary, Octal, and Hex literal syntax
  - Float literal syntax: 1.0 and 1.0e3
  - `round/1` and `trunc/1`
  - `is_integer/1`, `is_float/1`, and `is_number/1`
- [Booleans](../../../reference/types/boolean.md)
  - `is_boolean/1`
- [Atoms](../../../reference/types/symbol.md)
  - `true`, `false`, and `nil` as special atoms
  - `is_atom/1`
- [Strings](../../../reference/types/string.md)
  - [UTF8](../../../reference/types/utf8.md)
  - Interpolation
  - Binaries and `is_binary/1`
  - `String.length/1` vs `byte_size/1`
    - `length` vs `size` rule for linear vs constant time respectively
- [Anonymous functions](../../../reference/concepts/anonymous_functions.md)
  -`is_function/1` and `is_function/2`
  - As closures
  - Variable [scope](../../../reference/concepts/scope.md)
  - Implicit [return values](../../../reference/concepts/return_values.md)
  - Guards
    - more than 1 clause and guard may be used
      - clauses must have same number of arguments
    - do not leak errors, the error just makes the guard fail
- [Lists](../../../reference/types/list.md)
  - `is_list/1`
  - `length/1`
  - `++` and `--` operators
  - [Immutability](../../../reference/concepts/immutability.md)
  - `hd/1` and `tl/1`
- [Tuples](../../../reference/types/tuple.md)
  - `elem/2`, `tuple_size/1`, `put_elem/3`
  - `:ok` and `:error` tuples
- [Operators](../../../reference/concepts/operators.md)
  - `++` and `--` for lists
  - `<>` for strings
  - `or`, `and`, `not` for booleans
    - short circuit behavior
  - `||`, `&&`, `!` for any types
    - short circuit behavior
  - `==`, `!=`, `===,` `!==,` `<=,` `>=,` `<`, and `>` for comparison
    - `===` differentiates floats from ints
    - type ordering for comparison: number < atom < reference < function < port < pid < tuple < map < list < bitstring
- [Pattern matching](../../../reference/concepts/pattern_matching.md)
  - `=` as match operator
    - variable binding on left hand side
    - `MatchError`
    - `lists` with cons operator and special case of empty list
      - cons can also be used to prepend to list
    - variables may be rebound
    -  use pin operator `^` to pattern match against previously bound value instead of rebinding
    - if the same variable is used multiple times in a pattern, it must match the same value
      - e.g., `{x, x} = {1, 2}` results in a `MatchError`
    - use `_` to ignore values in a pattern match. its value can never be read.
- [Conditionals](../../../reference/concepts/conditionals.md)
  - `case`
    - compare a value against patterns
    - guards can be used
    - `CaseClauseError`, or catch all (`_` or `value`, `other`, etc.)
  - `cond`
    - evaluate multiple conditionals for truthiness (like "else if"s)
    - `CondClauseError` or catch all (`true`)
  - `if` and `unless`
    - evaluate a single conditional for truthiness
    - optionally supports `else`
  - TODO resume at https://elixir-lang.org/getting-started/case-cond-and-if.html#doend-blocks

### Other concepts

#### Functional

- [Higher-order functions](../../../reference/concepts/higher_order_functions.md)
- [Pipelines](../../../reference/concepts/pipelines.md)
- [Recursion](../../../reference/concepts/recursion.md)
- Tail Call Optimization

#### Platform-specific

- BEAM VM
- Processes
- Agent Concurrency Model
- Erlang Interoperation
- String vs Charlist
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
- TODO: more

#### Modules

- Naming
- Attributes
  - Redefining
- Alias
- Require
- Import
- Use
- Dynamic
- TODO: more

#### Functions
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
- Expression results
- Recursion
- Multi-clause functions

#### General

- Control Structures
- Literal Forms of data structures (lists, keyword lists, maps)
- Bitwise functions and operators
- Case vs Cond vs Multiple function clauses
- TODO: more

#### Basic Types

- List
  - Charlist
    - Codepoints
      - `?` operator
  - iodata
  - chardata
  - List Comprehensions
    - Generators
    - Filters
    - Into
- Reference
- PID
- Regex

#### Complex Types

- Keyword Lists
  - Role in function option gathering
- Maps
- Structs
- Ranges
- Streams
- Ports

#### Protocols

- Protocols as Polymorphism
- Protocols on data types
- Protocols on structs

#### Behaviours

- TODO: more

#### Sigils

- TODO: more

#### Error Handling

- Let it crash
- Try, Catch, Rescue

#### IO and the Filesystem

- TODO: more

#### Typespecs

- TODO: more

#### Metaprogramming

- Abstract Syntax Tree
- Macro
- TODO: more

## Concept interpretation

TODO: Flesh out how the above general concepts apply to concept exercises in the Elixir track.

The concept exercises use the following concepts:

| concept | interpretation |
| --- | --- |
| `default-arguments` | Extract Elixir specifics from [official guide](https://elixir-lang.org/getting-started/modules-and-functions.html#default-arguments). |
