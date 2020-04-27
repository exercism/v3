Nice work! Pattern matching is a used throughout Elixir, and it will make an appearance in nearly every other exercise! We did not yet go into detail on several related topics:

- Lists and tail recursion
- Multi-clause functions
- Guard clauses
- Matching on strings and character lists
- The pin operator

## Key Points

Maps are named structs, so you can use a map pattern to match on struct data
Underlines are used on variables that should be ignored
Pattern matching occurs inside `case` statements
You cannot make function calls on the left side of a match
Pattern matches are often defined in the function arguments


## Gotchas

There are a few tricks that may confuse you when it comes to pattern matching.

- `%{}` matches _any_ map or struct (full or empty)
- `[]` matches only an _empty_ list
- You cannot use the `_` (or variables beginning with `_`) anywhere outside a match.
