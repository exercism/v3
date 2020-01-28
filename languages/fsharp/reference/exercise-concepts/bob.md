# Concepts of bob

[Example implementation](https://github.com/exercism/fsharp/blob/master/exercises/bob/Example.fs)

## General

- functions: used as the main entry point for the exercise
- function arguments: input strand is passed as an argument
- methods: calling the `Char.IsLetter`, `String.IsNullOrEmpty` method and several other `string` methods
- return values: returning a value from a function
- implicit returns: the last expression is automatically returned from a function
- type annotations: annotating the string parameter
- type inference: automatically infer the type of the functions and values
- scoping: using whitespace to define scope
- modules: the functions are defined in a module
- imports: import types through `open` statements
- namespaces: knowing where to find the `Char` class
- assignment: assigning values
- higher order functions: passing `Char.IsLetter` as an argument to `Seq.exists`
- collection filtering: using `Seq.exists` to see if the string contains a letter
- equality operators: `=` used to compare strings
- boolean operators: `&&` used to combine boolean expressions
- strings: a `string` passed as the single input parameter
- booleans: a `bool` is used as the return value of the various string type tests

## Approach: if-else

- conditionals using if-elif-else: conditionally execute logic using an `if/elif/else` statement

## Approach: inner functions

- nested functions: defining helper functions inside tested function

## Approach: active patterns

- active patterns: defining active patterns for the various responses
- pattern matching: matching on the active patterns

## Approach: pattern matching

- pattern matching: matching on the input
- guard clauses: adding guard clauses to the pattern matching
- discards: ignore the pattern matches for the guard clauses to work

## Approach: regular expressions

- conditionals using if-elif-else: conditionally execute logic using an `if/elif/else` statement
- regular expressions: checking the phrase types using regular expressions
