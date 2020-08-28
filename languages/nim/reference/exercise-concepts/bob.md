# Concepts of protein-translation

[Example Implementation](https://github.com/exercism/nim/blob/master/exercises/bob/example.nim)

## General:

- importing modules
  - importing specific _things_ (not a technical term)
- implicit stdlib
- proc definition
  - parameters
  - typed
  - return type
- for loop
- using iterators
- if/elif/else statement
- boolean operator
- calling routines
- `return`
- `var`
- type inference
- cmp operators
- UFCS
- strings
  - initialization of literals
- documentation comment `##`
---

## Types

### `string`
- literal construction (`"`)
- iterating over the `char`s in a string
- a string is an `openarray` of `char`s (accesing a string results in a `char`)

### `char`
- literal construction (`'`)

### `bool`
- boolean keyword operators (and, not, or)
- `==`
  - `!=`
- is

---

## Specific Approaches

### Implicit return

[Example Implementation](https://exercism.io/tracks/nim/exercises/bob/solutions/fd46a50ebb2f47b8b415cc046ca7f65d)

- if expression
- implicit return

### `parseutils` module

[Example Implementation](https://exercism.io/tracks/nim/exercises/bob/solutions/5eeba8cf35ff469e8ac732e9abe62d51)

- multiline comment
- sets
- BackWardIndex
- openarray access with `[]`
- name parameters

### Implicit `result` variable

[Example Implementation](https://exercism.io/tracks/nim/exercises/bob/solutions/fdfdef2cedac4324a7c1f49545ae9188)

- implicit `result` variable

### Types

[Example Implementation](https://exercism.io/tracks/nim/exercises/bob/solutions/b3f58e77a19d4293be369db4f738084e)

- types
- tuples
  - vs. objects

### Generic parameters

[Example Implementation](https://exercism.io/tracks/nim/exercises/bob/solutions/e70f5bc5f63c4692a947fa121c8fdb40)

- generics
- anon. procs
- proc types

### `import as`

[Example Implementation](https://exercism.io/tracks/nim/exercises/bob/solutions/03b007333a7b489db24c6e0c9e07908b)

- `import as`
