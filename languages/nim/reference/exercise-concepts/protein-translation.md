# Concepts of protein-translation

[Example Implementation](https://github.com/exercism/nim/blob/master/exercises/protein-translation/example.nim)
(Note: Excpetion raising ignored, as it isn't tested for)

## General:

- importing libraries
- stdlib
- const/var/let
  - mutability vs. immutability
  - compile-time vs. runtime
- strings
- table constructor
  - multiple keys at one time (as seen [here](https://exercism.io/tracks/nim/exercises/protein-translation/solutions/fc829b982b274a80883f7f82d6e8faaf))
    <br />
    eg. `{"nim", "Nim": "awesome"}`
- method call syntax
- func/proc definiton
- routine exporting
- explicitly defining type of param
- defining specific versions of generic types `seq[T]`
- seq
- if statment
- infix operator
- for loop (loop in general :man_shrugging:)
- break (named for blocks)
- implicit system import (?)
- system procs (?)
- default parameters
- openarray accessing with `[]`
- slicing
  - `..<`/`..^`
- arithmetic (too granular?)
- table access `[]`
- equality comparison
- if/elif/else
- `return`
- implicit result definition
- command syntax (`result.add peptide`)

---
## Specific approaches

### Case Statment
[Example Implementation](https://exercism.io/tracks/nim/exercises/protein-translation/solutions/1078c01ba467400881b40827ffd1b84f)
- case statement
- of branch

### Iterator
- iterator vs. proc
- `yield` statement
