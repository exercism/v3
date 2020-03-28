# Common Lisp Reference

This is a work-in-progress document listing the concepts comprising the Common
Lisp programming language. I'm currently using [this document][csharp-example]
as template.

## Concepts
### General
  - [Anonymous Functions][anonymous-functions]
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
  - [Functions][functions]
    - Default Arguments
    - Keyword Arguments
    - Optional Arguments
    - Rest Arguments
    - Multiple Value Returns
  - [Higher Order Functions][higher-order-functions]
  - [Nested Functions][nested-functions]
  - Packages
  - [Recursion][recursion]
  - [Sameness][sameness]
    - Same Memory (`eq`)
    - Same Value Primitives (`eql`)
    - Same Value Objects (`equal`)
    - Lenient Sameness (`equalp`)
    - Type Specific (`=`, `char=`, `string-equal`, etc)
  - [Variables][variables]
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
  - [Classes][classes]
  - Generic Functions
  - [Methods][methods]
  - [Multiple Dispatch][multiple-dispatch]
  - [Multiple Inheritance][inheritance]
  - [Objects][objects]
  - Slots
  
### Conditions & Restarts (Needs Some Work)
  - Conditions
  - Handlers
  - Restarts
  - Signalling

### Types
  - [Booleans][bool]
  - [Characters][char]
  - [Hash Tables][hash-map]
  - [Numbers][number]
    - Complex
    - [Floats][floating-point-number]
    - [Integers][integer]
    - Rationals
  - Sequences
    - [Arrays][array]
    - Conses
      - [Association Lists][map]
      - [Lists][list]
      - Property Lists
      - [Set][set]
      - Trees
    - Vectors
  - Streams
  - [Strings][string]
  - [Structures][struct]

## Exercise Concepts
| Concept | Learning Objectives |
| ------- | ------------------- |
| [`sexpr`][sexpr] | Teach the student about S-expressions, the base unit of all Lisp code. The student should be able to distinguish an `atom` from a `cons` and know how to access the `car` and `cdr` of a cons pair. |

[csharp-example]: ../../csharp/reference/README.md
[anonymous-functions]: ../../../reference/concepts/anonymous_functions.md
[arithmetic]: ../../../reference/concepts/arithmetic.md
[assignment]: ../../../reference/concepts/assignment.md
[comments]: ../../../reference/concepts/comments.md
[conditionals]: ../../../reference/concepts/conditionals.md
[constants]: ../../../reference/concepts/constants.md
[enumeration]: ../../../reference/concepts/enumeration.md
[expressions]: ../../../reference/concepts/expressions.md
[functions]: ../../../reference/concepts/functions.md
[higher-order-functions]: ../../../reference/concepts/higher_order_functions.md
[nested-functions]: ../../../reference/concepts/nested_functions.md
[recursion]: ../../../reference/concepts/recursion.md
[sameness]: ../../../reference/concepts/sameness.md
[variables]: ../../../reference/concepts/variables.md
[classes]: ../../../reference/concepts/classes.md
[methods]: ../../../reference/concepts/methods.md
[multiple-dispatch]: ../../../reference/concepts/multiple-dispatch.md
[inheritance]: ../../../reference/concepts/inheritance.md
[objects]: ../../../reference/concepts/objects.md
[bool]: ../../../reference/types/boolean.md
[char]: ../../../reference/types/char.md
[hash-map]: ../../../reference/types/hash_map.md
[number]: ../../../reference/types/number.md
[floating-point-number]: ../../../reference/types/floating_point_number.md
[integer]: ../../../reference/types/integer.md
[array]: ../../../reference/types/array.md
[map]: ../../../reference/types/map.md
[list]: ../../../reference/types/list.md
[set]: ../../../reference/types/set.md
[string]: ../../../reference/types/string.md
[struct]: ../../../reference/types/struct.md

[sexpr]: ../exercises/concept/sexpr
