# Erlang Reference

## Concepts

### Beginner

- [Functions](../../../reference/concepts/functions.md)
  - [Anonymous functions](../../../reference/concepts/anonymous_functions.md)
  - [Higher-order functions](../../../reference/concepts/higher_order_functions.md)
  - Exported functions
  - Private/helper functions
  - [Recursion](../../../reference/concepts/recursion.md)
  - Tail recursion
  - Arity
    - Different arity, same name => different functions
  - [Pure functions](../../../reference/concepts/pure_functions.md)
  - Returning a value
- [Variables](../../../reference/concepts/variables.md)
  - [Immutability](../../../reference/concepts/immutability.md)
  - Binding
- [Pattern matching](../../../reference/concepts/pattern_matching.md)
  - In function clauses
  - In `case` clauses
- [Operators](../../../reference/concepts/operators.md)
  - [Boolean logic](../../../reference/concepts/boolean_logic.md) operators: `and`, `or`, `andalso`, `orelse`, `,`, `;`
  - [Aritmetic](../../../reference/concepts/arithmetic.md) operators: `+`, `-`, `*`, `div`, `/`, `rem`, and `mod`
- Guards
  - `if`
  - In function clauses
  - In `case` clauses
- Types and data structures
  - Atoms
  - Boolean atoms
  - Integers
    - Characters
  - Floats
  - Tuples
  - Lists
    - Strings
    - Basic `string` module functions
    - [List comprehensions](../../../reference/concepts/list_comprehension.md)
    - Basic `lists` module functions
    - List efficiency tips
  - Binaries
  - Maps
- Building strings (basic use of `io_lib`)
- [Macros](../../../reference/concepts/macros.md)
- Basic `erl` [REPL](../../../reference/concepts/repl.md) commands

### Intermediate

- More data structures
  - Proplists
  - Records
- [Concurrency](../../../reference/concepts/concurrency.md)
  - Processes
  - [Actor model](../../../reference/concepts/actor_model.md)
  - Event loops
- Errors and traps
  - Linking processes
  - Monitoring processes
- OTP Patterns
  - `gen_server`
  - `gen_statem`
  - Supervision trees
  - Child specifications
- Type specifications
  - Dialyzer
- OTP and `rebar3` application structure
- Sockets

### Advanced

- ETS and DETS
- Common test
- Mnesia
