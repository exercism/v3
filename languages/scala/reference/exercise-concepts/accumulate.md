# Concepts of accumulate

[Example implementation](https://github.com/exercism/scala/blob/master/exercises/accumulate/example.scala)

## General

- class: used as class wrapper for exercises
- functions: used as main entry point for the exercise
- function arguments: input function and list passed as arguments
- lambda: passed as input function and called from body of function
- return values: return a value from a function
- generics: use a type parameter to specialize a type : `F[A]`
- recursion: function calling the function to process the list items
- list: contains several items of the same type (arrays)
- pattern matching: conditionally execute logic using a `match` statement
- Nil: short cut for empty list
- list - pattern matching: deconstruct list into head and tail
