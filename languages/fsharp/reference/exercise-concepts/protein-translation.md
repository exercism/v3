# Concepts of protein-translation

[Example implementation](https://github.com/exercism/fsharp/blob/master/exercises/protein-translation/Example.fs)

## General

- functions: used as the main entry point for the exercise
- function arguments: rna is passed as an argument
- methods: calling helper method that is responsible for the mapping
- return values: returning a value from a function
- implicit returns: the last expression is automatically returned from a function
- type inference: automatically infer the type of the functions and values
- modules: the functions are defined in a module
- imports: import types through `open` statements
- namespaces: knowing where to find the `String` class
- pattern matching: matching on the codon
- default match: using `_` to catch codons that don't match
- exceptions: using `failwith` to handle invalid codons passed as input
- anonymous functions: a lambda is use to filter until the stopping point
- partial application: partially applying arguments to functions to return a new function
- pipeline: using the `|>` operator to construct a pipeline
- collection mapping: using `Seq.map` to map codons to proteins
- collection iteration: using `Seq` module functions to iterate over collection
- collection chunking: using `Seq.chunkBySize` to break collections into chunks
- equality operators: `<>` used to find the stopping point for the translations
- function composition: using `>>` to compose functions
