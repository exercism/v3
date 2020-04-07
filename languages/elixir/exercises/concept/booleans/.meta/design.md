# Design

## Goal

The goal of this exercise is to teach the student the basics of booleans and logicical expressions in Elixir

## Learning objectives

- Know what a variable is.
- Know how to define a named function.
- Know how to define a function with parameter variables.
- Know how to return a value from a function.
- Know how to define a boolean.
- Know how to use logical operators on booleans.
  - `and/2`, `or/2`, `not/1`
- Know how to name functions that return boolean values.

## Out of scope

- Default parameters.
- Truthy logical comparisons using `&&/2`, `||/2`, `!/1`
- Single-line functions
- Booleans as special atoms

## Prerequisites

This exercise's prerequisites are:

- `basics`: needs to be able to define named functions, return values

## Representer

This exercise does not require any specific representation logic to be added to the [representer][representer].

## Analyzer

This exercise could benefit from the following rules added to the the [analyzer][analyzer]:

- Verify that the functions use `and/2`, `or/2`, `not/1` rather than `&&/2`, `||/2`, `!/1`

[analyzer]: https://github.com/exercism/elixir-analyzer
[representer]: https://github.com/exercism/elixir-representer
