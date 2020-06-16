# Elixir reference

## Concepts

Below are the concepts that have been identified in Elixir for use in Exercism.
The initial breakdown of these concepts, including the ordering, is based on the elixir-lang.org [Getting Started Guide](https://elixir-lang.org/getting-started/introduction.html).

### Useful information not tested by exercises

- `iex`
  - `h/0`, plus `h/1`, `i`, `v`, etc.
- `elixir script.exs` to execute a script
- `IO.puts/1` and `IO.inspect/2`
- Inspect a string's codepoints with `str <> <<0>>` or `IO.inspect(str, binaries: :as_binaries)`

### elixir-lang.org Getting Started Guide concept extraction

- [Arithmetic](../../../reference/concepts/arithmetic.md)
  - `+`, `-`, `*`, and `/` operators
  - `/` always returns a [`Float`](../../../reference/types/floating_point_number.md)
  - `div` and `rem` for integer division and modulo
  - Binary, Octal, and Hex literal syntax
  - Float literal syntax: `1.0` and `1.0e3`
  - `round/1` and `trunc/1`
  - `is_integer/1`, `is_float/1`, and `is_number/1`
- [Booleans](../../../reference/types/boolean.md)
  - `is_boolean/1`
- [Atoms](../../../reference/types/symbol.md)
  - `true`, `false`, and `nil` as special atoms
  - `is_atom/1`
- [Strings](../../../reference/types/string.md)
  - Interpolation
  - Binaries and `is_binary/1`
  - `String.length/1` vs `byte_size/1`
    - `length` vs `size` rule for linear vs constant time respectively
- [Anonymous functions](../../../reference/concepts/anonymous_functions.md)
  - `is_function/1` and `is_function/2`
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
      - use pin operator `^` to pattern match against previously bound value instead of rebinding
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
    - `do`/`end` blocks are a syntactic convenience over the keyword syntax
- [Character encoding](../../../reference/concepts/character_encoding.md)
  - Codepoints
    - `?` operator to return a character's codepoint. E.g., `?a == 97`
    - `\u` notation to represent a unicode codepoint in a string (as hex).
      - e.g, `"\u0061" == "\u{61} == "a"`
  - [UTF8](../../../reference/concepts/utf8.md) encoding used to represent unicode codepoints in binaries
- [Bitstrings](../../../reference/types/bit.md)
  - `<<>>` syntax
  - contiguous sequence of bits in memory
  - number of bits specified with `::n` or `::size(n)` syntax.
    - E.g., `<<2::3>> == <<2::size(3)>>` and `<<2::3>> == <<0::1, 1::1, 0::1>>`
    - Defaults to 8. E.g, `<<42::8>> == <<42>>`
  - truncates extra bits from left. E.g., one byte stores 0..255 so `<<257>> == <<1>>`
  - `is_bitstring/1`
- [Binaries](../../../reference/types/bytes.md)
  - bitstrings where the number of bits is divisible by 8 (contains all full bytes)
  - `is_binary/1` vs `is_bitstring/1`
  - strings are binaries are bitstrings, but not all bitstrings are binaries and not all binaries are (valid) strings
  - `<>` concatenation for binaries (including strings)
  - pattern matching:
    - `<<x, y, z>> = <<0, 1, 2>>` defaults to matching one byte, use `::n` to specify bits
    - `<<x, y::binary>> = <<0, 1, 2>>` uses `binary` to match the rest of a binary
      - `x` is a codepoint (integer), `y` is a binary. e.g., `x == 0 and y == <<1, 2>>`
    - `<<x::binary-size(2), y::binary>> = <<0, 1, 2>>` uses `binary-size(2)` to match 2 bytes
    - `<<x, y::binary>> = "hello"` matches on strings since strings are binaries
    - `<<x::utf8, y::binary>> = "über"` uses `::utf8` match utf8 codepoints instead of a single byte
- Charlists
  - list of integers where all the integers are valid code points
  - common when interfacing with erlang; otherwise they are not idiomatic
  - `to_charlist/1` and `to_string/1`
- [Associative Data Structures](../../../reference/types/dictionary.md)
  - Keyword lists
    - list of tuples where the first item of the tuple (the key) is an atom
    - special syntax: `[{:a, 1}, {:b, 2}] == [a: 1, b: 2]`
    - keys may be duplicated; the first key's value is used
    - elements are ordered, unlike maps
    - when a keyword list is the last argument to a function, the brackets may be omitted
    - primarily used to pass options to functions
    - generally impractical to pattern match keyword lists as it depends on the order of the list
    - has linear performance characteristics of lists
    - `Keyword` module
  - TODO resume from <https://elixir-lang.org/getting-started/keywords-and-maps.html#maps>
  - Maps
  - Nested data structures

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
- elixir mix
  - directory structure
  - mix tasks
    - format
    - clean
    - test
- Naming conventions
- Eager Computation
- Lazy Computation
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

- Literal Forms of data structures (lists, keyword lists, maps)
- Bitwise functions and operators
- Case vs Cond vs Multiple function clauses
- TODO: more

#### Basic Types

- List
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

| concept                          | interpretation                                                                                            |
| -------------------------------- | --------------------------------------------------------------------------------------------------------- |
| `access-behaviour`               | Brief overview of behaviours, using access behaviour with maps, knowing it can be used with keyword lists |
| `anonymous-functions`            | Intro to anonymous functions, functions as data                                                           |
| `atoms`                          | Intro to Elixir atom type.                                                                                |
| `basics`                         | Introduction to functions, modules, variables, returning values, integers, invoking functions.            |
| `binaries`                       | Introduction to binaries as a specialization on the bitstring type, constructing and matching             |
| `bitstrings`                     | Introduction to bitstrings and constructing and matching on binary data                                   |
| `bit manipulation`               | Introduction to bit manipulation using the Bitwise module functions                                       |
| `booleans`                       | Introduction to the boolean type and strict boolean operators -- and/2, or/2, not/1                       |
| `default-arguments`              | Introduction to default arguments in named functions, function headers                                    |
| `errors`                         | Introduction to error handling in elixir (try, rescue)                                                    |
| `exceptions`                     | How to define, use exceptions                                                                             |
| `closures`                       | How to implement closures in Elixir                                                                       |
| `conditionals`                   | Intro to Elixir `cond/1` function.                                                                        |
| `floating-point-numbers`         | How to use floating point numbers to represent real numbers                                               |
| `guards`                         | What guards are, how to use guards in function heads                                                      |
| `lists`                          | Introduction to the lists type basic list functions -- hd/1, tl/1, length/1, in/2                         |
| `maps`                           | Introduction to the map data type                                                                         |
| `module-attributes-as-constants` | Introduction to using module attributes as constants                                                      |
| `multiple-clause-functions`      | Named function can be overloaded and each attempted to invoke until one succeeds                          |
| `pattern-matching`               | Basic knowledge of pattern matching using `=/2` and on function parameters                                |
| `recursion`                      | How to write basic recursive functions                                                                    |
| `string-literals`                | Introduction to strings in code enclosed by double quotes.                                                |
| `strings`                        | How to do string processing, concatenation, interpolation, and multiline strings.                         |
| `structs`                        | Intro to structs: definition, fields, enforcing keys                                                      |
| `static-access-operator`         | accessing map and struct fields with the `.`                                                              |
| `tail-call-recursion`            | How to efficiently perform recursion in Elixir to manage function call stacks and use accumulators        |
| `tuples`                         | Introduction to the tuple data type                                                                       |
| `errors`                         | Introduction to errors in elixir and patterns to communicate function success/failure                     |
| `try-rescue`                     | Use of the try-rescue construct in Elixir                                                                 |
