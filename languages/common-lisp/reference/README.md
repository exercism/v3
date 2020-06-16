# Common Lisp Reference

This is a work-in-progress document listing the concepts comprising the Common
Lisp programming language. I'm currently using [this document][csharp-example]
as template.

A more formal, machine-readable version of this concept list exists as
[concepts.csv][concepts]

## Concepts

### General

- [Arithmetic][arithmetic]
  - Prefix Notation
- [Assignment][assignment]
  - Generic Setters
- [Comments][comments]
  - Line
  - Block
  - Conventions
- [Conditionals][conditionals]
  - One Branch (`and`, `or`, `when`, `unless`)
  - Two Branch (`if`)
  - More Branches (`cond`, `case`)
- [Constants][constants]
- [Enumeration][enumeration]
  - Loop Macro
  - Do (`do`, `do*`, `dotimes`, `dolist`)
- [Expressions][expressions]
  - S-Expressions
- Packages
- [Sameness][sameness]
  - Same Memory (`eq`)
  - Same Value Primitives (`eql`)
  - Same Value Objects (`equal`)
  - Lenient Sameness (`equalp`)
  - Type Specific (`=`, `char=`, `string-equal`, etc)
- [Truthy and Falsy][truthy-and-falsy]
- [Variables][variables]
  - Global (`defparameter`, `defvar`)
  - Local (`let`, `let*`)

### Functions

- [Anonymous Functions][anonymous-functions]
- [Function Definition][functions]
  - Default Arguments
  - Keyword Arguments
  - Optional Arguments
  - Rest Arguments
- [Higher Order Functions][higher-order-functions]
- Multiple Values
- [Nested Functions][nested-functions]
- [Recursion][recursion]

### DSLs

- Format
  - Basic
  - Conditionals
  - Iteration
  - Language
  - Miscellaneous
  - Numbers
  - Tables

### CLOS (Needs Some Work)

- [Classes][classes]
- Generic Functions
- [Methods][methods]
- Metaobject Protocol (MOP)
- [Multiple Dispatch][multiple-dispatch]
- [Multiple Inheritance][inheritance]
- [Objects][objects]
- Slots

### Conditions & Restarts (Needs Some Work)

- Conditions
- Handlers
- Restarts
- Signalling

### Macros (Needs Lots of Work)

- Code as Data (`defmacro`, `` ` ``, `,`, `,@`)
- Unique Symbols (`gensym`)
- Reader Macros (`set-macro-character`, `#.` `#n=`, etc)

### I/O

- Printing
- Reading

### Sequences

- [Arrays][array]
- [Association Lists][map]
- Circular Lists
- [Lists][list]
- Property Lists
- [Strings][string]
- Vectors

### Numbers

- Complex
- [Floats][floating-point-number]
- [Integers][integer]
- Rationals

### Types

- [Booleans][bool]
- [Characters][char]
- [Cons][cons]
- [Hash Tables][hash-map]
- [Sets][set]
- Streams
- [Structures][struct]
- Symbols
- Trees

## Implemented Concept Exercises

| Exercise           | Concepts                                     |
| ------------------ | -------------------------------------------- |
| [`basics`][basics] | `comments`, `expressions`, `cons`, `symbols` |

[anonymous-functions]: ../../../reference/concepts/anonymous_functions.md
[arithmetic]: ../../../reference/concepts/arithmetic.md
[array]: ../../../reference/types/array.md
[assignment]: ../../../reference/concepts/assignment.md
[bool]: ../../../reference/types/boolean.md
[char]: ../../../reference/types/char.md
[classes]: ../../../reference/concepts/classes.md
[comments]: ../../../reference/concepts/comments.md
[conditionals]: ../../../reference/concepts/conditionals.md
[cons]: ../reference/types/cons.md
[constants]: ../../../reference/concepts/constants.md
[csharp-example]: ../../csharp/reference/README.md
[enumeration]: ../../../reference/concepts/enumeration.md
[expressions]: ../../../reference/concepts/expressions.md
[floating-point-number]: ../../../reference/types/floating_point_number.md
[functions]: ../../../reference/concepts/functions.md
[hash-map]: ../../../reference/types/hash_map.md
[higher-order-functions]: ../../../reference/concepts/higher_order_functions.md
[inheritance]: ../../../reference/concepts/inheritance.md
[integer]: ../../../reference/types/integer.md
[list]: ../../../reference/types/list.md
[map]: ../../../reference/types/map.md
[methods]: ../../../reference/concepts/methods.md
[multiple-dispatch]: ../../../reference/concepts/multiple-dispatch.md
[nested-functions]: ../../../reference/concepts/nested_functions.md
[number]: ../../../reference/types/number.md
[objects]: ../../../reference/concepts/objects.md
[recursion]: ../../../reference/concepts/recursion.md
[sameness]: ../../../reference/concepts/sameness.md
[set]: ../../../reference/types/set.md
[string]: ../../../reference/types/string.md
[struct]: ../../../reference/types/struct.md
[truthy-and-falsy]: ../../../reference/concepts/truthy_and_falsy.md
[variables]: ../../../reference/concepts/variables.md
[concepts]: concepts.csv
[basics]: ../exercises/concept/basics
