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
  - [Functions][func]
    - Default Arguments
    - Keyword Arguments
    - Optional Arguments
    - Rest Arguments
  - [Higher Order Functions][high]
  - [Nested Functions][nest]
  - Packages
  - [Recursion][recu]
  - [Sameness][same]
    - Same Memory (`eq`)
    - Same Value Primitives (`eql`)
    - Same Value Objects (`equal`)
    - Lenient Sameness (`equalp`)
    - Type Specific (`=`, `char=`, `string-equal`, etc)
  - [Variables][vari]
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
  - [Classes][clas]
  - Generic Functions
  - [Methods][meth]
  - [Multiple Dispatch][mult]
  - [Multiple Inheritance][inhe]
  - [Objects][obje]
  - Slots
  
### Conditions & Restarts (Needs Some Work)
  - Conditions
  - Handlers
  - Restarts
  - Signalling

### Types
  - [Booleans][bool]
  - [Characters][char]
  - [Hash Tables][hash]
  - [Numbers][numb]
    - Complex
    - [Floats][floa]
    - [Integers][inte]
    - Rationals
  - Sequences
    - [Arrays][arra]
    - Conses
      - [Association Lists][maps]
      - [Lists][list]
      - Property Lists
      - [Sets][sets]
      - Trees
    - Vectors
  - Streams
  - [Strings][stri]
  - [Structures][stru]

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
[func]: ../../../reference/concepts/functions.md
[high]: ../../../reference/concepts/higher_order_functions.md
[nest]: ../../../reference/concepts/nested_functions.md
[recu]: ../../../reference/concepts/recursion.md
[same]: ../../../reference/concepts/sameness.md
[vari]: ../../../reference/concepts/variables.md
[clas]: ../../../reference/concepts/classes.md
[meth]: ../../../reference/concepts/methods.md
[mult]: ../../../reference/concepts/multiple-dispatch.md
[inhe]: ../../../reference/concepts/inheritance.md
[obje]: ../../../reference/concepts/objects.md
[bool]: ../../../reference/types/boolean.md
[char]: ../../../reference/types/char.md
[hash]: ../../../reference/types/hash_map.md
[numb]: ../../../reference/types/number.md
[floa]: ../../../reference/types/floating_point_number.md
[inte]: ../../../reference/types/integer.md
[arra]: ../../../reference/types/array.md
[maps]: ../../../reference/types/map.md
[list]: ../../../reference/types/list.md
[sets]: ../../../reference/types/set.md
[stri]: ../../../reference/types/string.md
[stru]: ../../../reference/types/struct.md
