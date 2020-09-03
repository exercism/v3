# Closures

## The Concept

A `closure` the use of a variable in an inner lexical scope from an outer lexical scope. Historically, this nomenclature was derived from [_λ-calculus_][wiki-lambda-calculus] and popularized by [_scheme_][wiki-scheme] ([source][wiki-closure]) to reference a function's open and closed variable bindings.

## What to cover

- **What a closure is.** What makes up a closure in your language?
- **How to define a clojure.** Does it occur transparently (like JavaScript)? Is it explicit (like PHP)? What syntax?
- **How to use a clojure.** What can you use it for? Dynamic functions, function composition, memoizing, callbacks?

## Implementations

| Track      | Exercise                              | Changes |
| ---------- | ------------------------------------- | ------- |
| JavaScript | [closures][implementation-javascript] | None    |

[implementation-javascript]: ../../languages/javascript/exercises/concept/closures/.docs/introduction.md
[wiki-lambda-calculus]: https://en.wikipedia.org/wiki/%CE%9B-calculus
[wiki-scheme]: https://en.wikipedia.org/wiki/Scheme_(programming_language)
[wiki-closure]: https://en.wikipedia.org/wiki/Closure_(computer_programming)
