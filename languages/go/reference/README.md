This list is a work in progress and is being discussed and expanded in [this issue](https://github.com/exercism/v3/issues/167).

# Go

This is a list of concepts of Go. They are categorized into `OOP`, `Functional` and `General`. The `Specialties` category tries to mention concepts that are somewhat special to Go. A concept can belong to multiple categories.

## Specialties

Concepts that are special to Go or have key differences to a vast majority of other languages.

- Channels
- Code Formatting
- Concurrency
- Constants
- context.Context
- Defer
- Empty Interface
- Errors and Error Handling
- Goroutines
- Modules
- Multiple Return Values
- Named Returns
- Nil
- Packages
- Panics
- Pointers
- Readers, Writers, Buffers, Pipes
- Recover
- Reflection
- Select
- Type Switch
- Unsafe
- WaitGroups
- Zero Values

## General concepts

- [Arithmetic](../../../reference/concepts/arithmetic.md)
- [Bitwise manipulation](../../../reference/concepts/bitwise_manipulation.md)
- [Boolean logic](../../../reference/concepts/boolean_logic.md)
- Channels
- Code Formatting
- context.Context
- [Comments](../../../reference/concepts/comments.md)
- Concurrency
- [Conditionals](../../../reference/concepts/conditionals.md)
- Debugging
- Defer
- Errors and Error Handling
- [Functions](../../../reference/concepts/functions.md)
- Globals
- Ignoring (return) values
- Init function
- JSON Handling
- [Loops](../../../reference/concepts/loops.md)
- [Methods](../../../reference/concepts/methods.md)
- Modules
- [Mutexes](../../../reference/concepts/locking.md)
- Packages
- Panics
- Pointers
- Nil
- Random (math/rand vs crypt/rand)
- Readers, Writers, Buffers, Pipes
- Recover
- Reflection
- Regular Expressions
- [Scope](../../../reference/concepts/scope.md)
- Switch
- Type Switch
- Unsafe
- [Variables](../../../reference/concepts/variables.md)
- WaitGroups
- Zero Values

## Object-oriented Concepts

> Although Go has types and methods and allows an object-oriented style of programming, there is no type hierarchy. The concept of "interface" in Go provides a different approach that we believe is easy to use and in some ways more general. There are also ways to embed types in other types to provide something analogous-but not identical-to subclassing. Moreover, methods in Go are more general than in C++ or Java: they can be defined for any sort of data, even built-in types such as plain, "unboxed" integers. They are not restricted to structs (classes). [Is Go an object-oriented language?](https://golang.org/doc/faq#Is_Go_an_object-oriented_language)

- Structs (Classes)
- Method Sets
- [Encapsulation](../../../reference/concepts/encapsulation.md)
- [State](../../../reference/concepts/state.md)
- [Mutation](../../../reference/concepts/mutation.md)
- [Composition](../../../reference/concepts/composition.md)
- Polymorphism (?)
- Interfaces

## Functional Concepts

> Go is not a functional language but has a lot of features that enable us to apply functional principles. [Functional Go](https://medium.com/@geisonfgfg/functional-go-bc116f4c96a4)

- [Anonymous Functions](../../../reference/concepts/anonymous_functions.md)
- [Function Composition](../../../reference/concepts/function_composition.md)
- Higher Order Functions
- Multiple Return Values
- Named Returns
- [Nested Functions](../../../reference/concepts/nested_functions.md)
- [Partial Application](../../../reference/concepts/partial_application.md)
- Pipelines (?)
- [Pure Functions](../../../reference/concepts/pure_functions.md)
- [Recursion](../../../reference/concepts/recursion.md)
- REPL (some community projects)
- [Type Inference](../../../reference/concepts/type_inference.md)

## Advanced

- Code generation (`go generate`)
- [plugins](https://golang.org/pkg/plugin/)
- [cgo](https://golang.org/cmd/cgo/)
- [Assembly in Go](https://goroutines.com/asm)
- [conditional compilation](https://dave.cheney.net/2013/10/12/how-to-use-conditional-compilation-with-the-go-build-tool)

## Patterns

### Concurrency

- synchronization for sharing memory (mutex, atomics)
- synchronization via communication (channels)
- data protected by `confinement` (`guard` pattern ?)
- for-select loop
- `fan-in`, `fan-out`

## Types

Types are not really concepts but it might be helpful to have as list of types Go has as some might need an extra introduction. Some are already added as a concept above, e.g. `channels`, `interfaces`, etc. as there are special concepts around some of Go's types.

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
- [string](../../../reference/types/string.md)
- struct
- time.Duration
- time.Time

## Tooling

- go doc / documentation, eg export & package documentation. Machine/human. Inline "why"
- golint
- go vet
- go test / writing tests
- benchmarking / profiling
- gofmt
- golangci-lint

## Resources used

- [The Go Programming Language Specification](https://golang.org/ref/spec)
- [Effective Go](https://golang.org/doc/effective_go.html)
- [Frequently Asked Questions (FAQ)](https://golang.org/doc/faq)
- [Concurrency Patterns](https://www.oreilly.com/library/view/concurrency-in-go/9781491941294/ch04.html)
