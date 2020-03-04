# Concepts of scrabble-score

[Example implementation](https://github.com/exercism/scala/blob/master/exercises/scrabble-score/example.scala)

## General

- object or case class: used as class wrapper for exercises
- methods: used as main entry point for the exercise and helper methods, and to access the string's `toUpper` method
- functions: each method is a function
- function arguments: input strands passed as arguments
- inline functions: a lambda is defined in the `flatMap` call
- return values: return a value from a function
- visibility: making values `private`
- scoping: single-line function scoping
- immutability: define immutable values using `val`
- pattern matching: a pattern match is used in the lambda
- math operators: `+` to calculate total score
- collections - folds: a fold is used to calculate the total score
- for-comprehension: a for-comprehensen is used to create a letter scores
- collection - accessors: getOrElse: retrieve a value given a key or use default value
- integers: an `Int` is used to represent a letter score and the total score
- characters: a `Char` is used to represent a letter
- strings: a `String` is used to represent the input word
- dictionaries: a `Map[Char, Int]` is used for the return value
- lists: a `List[(String, Int)]` is used to store the letter scores
- tuples: a `(String, Int)` tuple is used to describe a letters worth
