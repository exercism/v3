# Go

<<<<<<< HEAD
## Specialties

- Channels
- Code Formatting
- Concurrency
- Defer
- Empty Interface
- Errors and Error Handling
- Modules
- Multiple Return Values
- Named Returns
- Nil
- Packages
- Panics
- Pointers
- Reflection
- Type Switch
- Unsafe
- Zero Values

## General concepts

- [Arithmetic](../../reference/concepts/arithmetic.md)
- [Bitwise manipulation](../../reference/concepts/bitwise_manipulation.md)
- [Boolean logic](../../reference/concepts/boolean_logic.md)
- Channels
- Code Formatting
- [Comments](../../reference/concepts/comments.md)
- Concurrency
- [Conditionals](../../reference/concepts/conditionals.md)
- Defer
- Errors and Error Handling
- [Functions](../../reference/concepts/functions.md)
- [Loops](../../reference/concepts/loops.md)
- [Methods](../../reference/concepts/methods.md)
- Modules
- [Mutexes](../../reference/concepts/locking.md)
- Packages
- Panics
- Pointers
- Nil
- Reflection
- [Scope](../../reference/concepts/scope.md)
- Type Switch
- Unsafe
- [Variables](../../reference/concepts/variables.md)
- Zero Values

## Object-oriented Concepts

> Although Go has types and methods and allows an object-oriented style of programming, there is no type hierarchy. The concept of "interface" in Go provides a different approach that we believe is easy to use and in some ways more general. There are also ways to embed types in other types to provide something analogous-but not identical-to subclassing. Moreover, methods in Go are more general than in C++ or Java: they can be defined for any sort of data, even built-in types such as plain, "unboxed" integers. They are not restricted to structs (classes). [Is Go an object-oriented language?](https://golang.org/doc/faq#Is_Go_an_object-oriented_language)

- Structs (Classes)
- Method Sets
- Encapsulation
- State
- Mutation
- Composition
- Polymorphism (?)
- Interfaces

## Functional Concepts

> Go is not a functional language but has a lot of features that enable us to apply functional principles. [Functional Go](https://medium.com/@geisonfgfg/functional-go-bc116f4c96a4)

- Anonymous Functions
- Function Composition
- Higher Order Functions
- Multiple Return Values
- Named Returns
- Nested Functions
- Partial Application
- Pipelines (?)
- Pure Functions (?)
- Recursion
- REPL (some community projects)
- Type Inference

## Types

- array (rarely used)
- bool
- channel
- function
- interface
- interface{} (empty interface)
- map
- nil
- Numeric Types (`uint` types, `int` types, `float` types, `complex` types, `byte` and `rune`)
- pointer
- slice
- string
- struct

## Resources used

- [The Go Programming Language Specification](https://golang.org/ref/spec)
- [Effective Go](https://golang.org/doc/effective_go.html)
- [Frequently Asked Questions (FAQ)](https://golang.org/doc/faq)
=======
Welcome to the workspace for Exercism v3's Go track!

This area will contain everything needed to launch the Go track, including:

- The new exercises and `config.json` file.
- Reference documentation that help explain Go concepts for which no appropriate online document could be found.
- Go-specific documentation for contributors.

## Preparation Status

Before we publicize requesting contribution for this language, the following steps should be done.

- [ ] Have a kick-off discussion between track maintainers
- [ ] [Write a Concept Exercise implementation guide](../../docs/maintainers/writing-a-concept-exercise-github-issue.md)
- [ ] [List out key Concepts for your language](../../docs/maintainers/determining-concepts.md)
- [ ] [Add GitHub issues for 20 Concept Exercises](../../docs/maintainers/writing-a-concept-exercise-github-issue.md)

## Readiness for Launch

Before launch, we need all of the following parts to be completed:

### Track Structure

- [ ] Implemented 20+ Concept Exercises
- [ ] [Updated `config.json`](../../docs/maintainers/migrating-your-config-json-files.md)
  - [ ] Added `version` key
  - [ ] Added online editor settings
    - [ ] Added `indent_style`
    - [ ] Added `indent_size`
  - [ ] Added Concept Exercises
  - [ ] Added Concepts for all Practice Exercises

### Representer

- [ ] Build Representer
- [ ] Deploy Representer

### Test Runner

- [ ] Build Test Runner
- [ ] Deploy Test Runner

## Extra magic

These extra steps will make your track better, but are optional.

### Analyzer

- [ ] Build Analyzer
- [ ] Deploy Analyzer
>>>>>>> b5829c8... Normalize ending of sentence
