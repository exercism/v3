# Concepts of space-age

[Example implementation](https://github.com/exercism/scala/blob/master/exercises/space-age/example.scala)

## General

- object or case class: used as class wrapper for exercises
- methods: used as main entry point for the exercise and helper methods
- functions: each method is a function
- function arguments: input strands passed as arguments
- return values: return a value from a function
- visibility: making values `private`
- scoping: use `{` and `}` to denote scoping as well as having single-line function scoping
- immutability: define immutable values using `val`
- assignment: assigning values, such as the orbital periods
- floating-point numbers: a `Double` is used for the orbital periods
- math operators: `/` to calculate space age

## Approach: partial functions

- partial functions: defining helper functions as partials
