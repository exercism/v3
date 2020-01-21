# Rust reference

## Concepts

The Rust concept exercises teach concepts from fundamentals through language mastery. They are classified and listed below.

### Fundamentals of Rust

These concepts are irreducable and must be mastered for further progress in the language.

- [Functions](../../../reference/concepts/functions.md)
- Test Suites
- Primitives 
- [Conditionals](../../../reference/concepts/conditionals.md)
- [Loops](../../../reference/concepts/loops.md)
- Iterator usage 
- Slices

### Intermediate Features

These concepts are more advanced than the fundamentals, but may be familiar from other languages.

- [Structs](../../../reference/types/struct.md)
- [Methods](../../../reference/concepts/methods.md)
- [Composition](../../../reference/concepts/composition.md)
- Enums
- [Generics](../../../reference/concepts/generics.md)
- `Vec<T>`
- `Hashmap<K, V>`
- Using External Crates / Libraries
- `match` and [Pattern matching](../../../reference/concepts/pattern_matching.md)
- [Anonymous functions](../../../reference/concepts/anonymous_functions.md)
- [Recursion](../../../reference/concepts/recursion.md)

### Unusual Language Features

These concepts are distinctive or unique to Rust.

- Ownership / Borrowing
- `String` vs `&str`
- `Option<T>`
- `Result<T, E>`
- Implementing an external (stdlib) trait, i.e. `Iterator`
- [Immutability](../../../reference/concepts/immutability.md)

### Advanced Concepts

- Writing your own trait
- Writing declarative macros
- Writing procedural macros
- `Box<T>`
- `Rc<T>` and `Arc<T>`
- `Cell<T>` and `RefCell<T>`
- `Fn`, `FnMut`, and `FnOnce`
- `unsafe`
- Parallelism
- [Higher-order functions](../../../reference/concepts/higher_order_functions.md)
- async

### Miscellaneous

- `Debug` and `Display`
- `Eq` and `Ord`
- `Add`, `Sub`, `Mul`, `Div`, etc.
- overflow/underflow functions


## Concept exercise mapping

While the ideal is to have a single concept exercise for each of the concepts listed above, which 
teaches exactly one concept and requires only its prerequisites, 
doing so is not entirely practical. For example, it would be difficult to design an exercise
which taught what functions are without requiring any knowledge of primitives; likewise,
it would be difficult to teach primitives, with a test suite, without any knowledge of functions.

exercise | prerequisites | teaches | narrative
--- | --- | --- | ---
`hello-world` | - | functions, test suites | 
`primitives` | `hello-world` | primitives | general nonexhaustive intro to Rust primitives: `bool`, `u32`, `i32`, `f64`
`conditionals` | `primitives` | conditionals | 
`loops` | `conditionals` | loops | `for` loops over a numeric range
`iterator-use` | `loops` | iterator use | `for` loops over a provided list; `"string".chars()`
`slices` | `loops` | slices |
`fundamentals` | `hello-world`, `primitives`, `conditionals`, `loops`, `iterator-ouse`, `slices` | - | probably something like `bob`: just demonstrates that students understand the basic building blocks of the language. 
`structs` | `fundamentals` | structs | just primitives for now
`methods` | `structs` | struct methods
`composition` | `structs` | struct composition | 
`enums` | `fundamentals` | the `enum` type
`generics` | `composition` | generics in structs |
`vec` | `generics` | `Vec<T>` |
`hashmap` | `generics` | `Hashmap<K, V>` |
`external-crates` | `fundamentals` | using external crates / libraries | might depend on structs or whatever depending on the specific design we come up with
`match` | `fundamentals` | `match` and pattern matching | might depend on structs or whatever depending on the specific design we come up with
`closures` | `fundamentals` | closures / anonymous functions | might depend on structs or whatever depending on the specific design we come up with
`recursion` | `fundamentals` | recursion | might depend on structs or whatever depending on the specific design we come up with

_...etc_

## Reference docs

Reference docs are written to help explain a particular concept to a student when no appropriate online document can be found. They will be used when creating exercises and as references in exercise documentation.

The following reference docs have been written:

_None_

## TODO

- Finish specifying concept exercises with specific dependencies and topics
- Find references in The Rust Book to as many of the concepts above as possible; link them, replacing exercism references
- Find references in the [general exercism reference section](../../../reference/README.md) for as many of the remaining concepts as possible; link them
- Write appropriate references for all remaining concepts; link them
