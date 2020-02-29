# Common Lisp Reference

Please note that this is a *very* WIP document. I'm currently using [this
document][csharp-example] as an example to base my work off of.

## Concepts
### General
  - [Anonymous Functions][anon]
  - [Arithmetic][arit]
    - Prefix Notation
  - [Assignment][assi]
    - Generic Setters
  - [Comments][comm]
    - Line
    - Block
    - Conventions
  - [Conditionals][cond]
    - One Branch (`and`, `or`, `when`, `unless`)
    - Two Branch (`if`)
    - More Branches (`cond`, `case`)
  - [Constants][cons]
  - [Enumeration][enum]
    - Loop Macro
    - Do (`do`, `do*`, `dotimes`, `dolist`)
  - [Expressions][expr]
    - S-Expressions
  - Functions
  - Higher Order Functions
  - Nested Functions
  - Packages
  - Recursion
  - Sameness
  - Variables
    - Global (`defparameter`, `defvar`)
    - Local (`let`, `let*`)

### Format
  - Basic
  - Conditionals
  - Iteration
  - Language
  - Miscellaneous
  - Numbers
  - Tables

### Loop (Needs Lots of Work)
  - Collecting
  - Hash Tables
  - Miscellaneous
  - Ranges
  - Sequences

### CLOS (Needs Some Work)
  - Classes
  - Generic Functions
  - Methods
  - Multiple Dispatch
  - Multiple Inheritance
  - Objects
  - Slots
  
### Conditions & Restarts (Needs Some Work)
  - Conditions
  - Handlers
  - Restarts
  - Signalling

### Types
  - Booleans
  - Characters
  - Hash Tables
  - Numbers
    - Complex
    - Floats
    - Integers
    - Rationals
  - Sequences
    - Arrays
    - Conses
      - Association Lists
      - Lists
      - Property Lists
      - Sets
      - Trees
    - Vectors
  - Streams
  - Strings
  - Structures

## Exercise Concepts
We should put a table here that go into the specifics to teach for each
concept. Also give the concepts nice names. I think we are using `.` for
sub-concepts. So things like `format.numbers` or maybe
`string-formatting.numbers`.

[csharp-example]: ../../csharp/reference/README.md
[anon]: ../../../reference/concepts/anonymous_functions.md
[arit]: ../../../reference/concepts/arithmetic.md
[assi]: ../../../reference/concepts/assignment.md
[comm]: ../../../reference/concepts/comments.md
[cond]: ../../../reference/concepts/conditionals.md
[cons]: ../../../reference/concepts/constants.md
[enum]: ../../../reference/concepts/enumeration.md
[expr]: ../../../reference/concepts/expressions.md

