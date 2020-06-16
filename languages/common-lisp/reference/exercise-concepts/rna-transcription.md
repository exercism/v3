# General Concepts of RNA Translation

- Constants / Variables: Used to define a mapping from DNA to RNA
  bases. "Earmuffs" with `*` and `+`
- String: DNA is given as a string (an array of characters)
- Characters: ASCII standard with most implementations extending this to Unicode
- Functions: Writing a function to convert from DNA to RNA one character at a
  time is common
- Sameness: Common lisp has a _lot_ of equality operators. Most useful here are:
  `eql` and `char=`
- Comments: We can hope for well commented code (this includes the use of
  docstrings!)
- Nested Functions: The base-pairing function could be local via `flet` or
  `labels`

# Base Paring

## Approach: Case Conditional

- Conditionals: The use of `case` of an exhaustive `ecase` allows an easy
  mapping between bases
- Expressions: Case is an expression, not a statement, so it's implicitly
  returned

## Approach: Association List

- Map: Pairs each DNA base with its biological RNA complement. Implemented here
  as an alist of cons cells: `((#\G . #\C) ...)` accessed via `assoc`

## Approach: Hash Map

- Hash Map: Same function as the alist, but implemented using a Hash Table
  accessed via `gethash`
- Generic Setters: Building the hash table means combining `gethash` with `setf`
  and showing off generic setters

# Sequence Traversal

## Approach: Loop Macro

- Enumeration: Common Lisp's `loop` macro makes it possible to loop through the
  chars in a string with `across`

## Approach: Recursion

- Recursion: Questionably efficient (tail calls aren't in the standard), but
  always an option

## Approach: Higher Order Functions

- Higher Order Function / Enumeration: The use of `(map 'string ...)` allows you
  to convert whole strings a character at a time
- Anonymous Functions: Some students may pass a `lambda` to `map`
