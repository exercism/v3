# General Concepts of Word Count

- Variables: Local variables with `let` or `let*` are commonplace
- String: Strings need to be split into a list of words. Cleaning is also done
  with `string-trim` and `string-downcase`
- Functions: Helper functions are commonly defined
- Sameness: Common lisp has a _lot_ of equality operators. Most useful here are:
  `equal` and `string=`
- Comments: We can hope for well commented code (this includes the use of
  docstrings!)
- Nested Functions: Often appear as string cleaners or serve other
  functions. Defined with `flet` or `labels`
- Higher Order Functions: The use of various functions like `remove-if` and
  `map` is common
- Anonymous Functions: Some students may pass a `lambda` to functions like `map`
- Lists: This exercise requires a result given as an `alist`
- Boolean Logic: Compositions of `and` and `or`. Helpfully short-circuiting

# String -> Words

## Approach: Library Import

- Regular Expressions: Needed if the student is using the `cl-ppcre`. Maybe this
  can be isolated to a "library concepts" section. It shouldn't pollute the main
  track.

## Approach: Home Rolled

- Enumeration: Common Lisp's `loop` macro makes it possible to loop through the
  chars in a string with `across`. `do` or `do*` can also be used for
  enumeration
- Conditions: Many of CL's conditionals can be used to detect separators in the
  string
- Recursion: A popular approach for eating a word at a time

# Word Counting

## Approach: Association List

- Map: List of cons cells is used to keep count of the words

## Approach: Hash Map

- Hash Map: Same function as the alist, but implemented using a Hash Table
  accessed via `gethash`
- Generic Setters: Building the hash table means combining `gethash` with `setf`
  and showing off generic setters
- Mutation: Hash Maps are mutated / updated as new words are scanned
