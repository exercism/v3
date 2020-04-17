# Concepts of book store

[Example implementation](https://github.com/exercism/fsharp/blob/master/exercises/book-store/Example.fs)

## General

- functions: used as the main entry point for the exercise
- function arguments: input strand is passed as an arguments
- modules: the functions are defined in a module
- visibility: use `private` keyword for helper functions that aren't publicly exposed
- recursion: using the `rec` function
- tail recursion: ensuring the implemention is protected against stack overflow errors
- methods: using many methods from the `List` class (ex: `append`, `take`, `rev`)
- properties: calling the `length` property of a `List`
- return values: returning a value from a function
- implicit returns: the last expression is automatically returned from a function
- type inference: automatically infer the type of the functions and values
- pattern matching: matching on the `Week` discriminated union
- default match: using `_` to catch codons that don't match
- assignment: assigning values
- anonymous functions: a lambda is use to sort and fold
- pipeline: using the `|>` operator to construct a pipeline
- collection mapping: using `List.map` to map groups to prices
- collection iteration: using `List` module functions to iterate over collection
- ranges: instantiate a range with `[ .. ]` syntax
- floating point numbers: a `decimal` is used to store the book prices
- math operations: `-`, `*`, and `/`
