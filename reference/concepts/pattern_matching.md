# Pattern matching

## The Concept

The ability to compare a value against a _pattern_, and conditionally executing code depending on which pattern the value successfully matches.
Various types of patterns are possible, such as value- or structural matching.

## What to cover

- **What sum types are in your language** What can they be used for? Do they have limitations? How are they different from enums? Does the compiler (if any) check for exhaustiveness?
- **Are guards supported?** Can guards be added to pattern matching clauses?
- **What patterns are available?** What data types can pattern matching be used on? How does one handle "all other cases?"

## Exercises

### Valentine date

This exercise chooses suitable options for a valentine's day date. The reference implementation (F#) teaches:

- How to do pattern matching on sum types.
- Constant, identifier and wildcard patterns.
- How to apply guards to patterns.
- What exhaustiveness checking in pattern matching is.

#### Implementations

| Track | Exercise                         | Changes |
| ----- | -------------------------------- | ------- |
| F#    | [strings][implementation-fsharp] | None    |

[type-char]: ./char.md
[implementation-fsharp]: ../../languages/fsharp/exercises/concept/discriminated-unions/.docs/introduction.md
