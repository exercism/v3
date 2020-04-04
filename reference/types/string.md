# String

## The Concept

A string is a sequence of [characters][type-char] (letters, digits, punctuation, etc.).

## What to cover

Tell a student what a string is in your language, how strings can be manipulated, and ensure they understand where to look for docs on strings.

- **Explain what strings are in your language?** Is it a sequence of bytes, UTF-8 characters or something else? Are strings objects or primitives?
- **How are strings brought together?** Do you join strings, add strings, concatenate strings, interpolate strings? It's worth giving a hint as to which is normal, or letting the student know all are ok!
- **How are strings split apart?** How do I extract a bit of a string?
- **Where do helper methods for strings live?** If I want to trim a string, or check it's length, how do I do that in your language? Are these string methods or functions that act on strings?

## Exercises

### Log Lines

This exercise extracts information from log lines. The reference implementation (C#) teaches:

- String concatenation
- Trimming
- Changing casing
- Extracting substrings

#### Implementations

| Track | Exercise                         | Changes |
| ----- | -------------------------------- | ------- |
| C#    | [strings][implementation-csharp] | None    |
| F#    | [strings][implementation-fsharp] | None    |
| Ruby  | [strings][implementation-ruby]   | None    |
| Python| [strings][implementation-python] | None    |
| Go    | [strings][implementation-go]     | None    |

[type-char]: ./char.md
[implementation-csharp]: ../../languages/csharp/exercises/concept/strings/.docs/introduction.md
[implementation-fsharp]: ../../languages/fsharp/exercises/concept/strings/.docs/introduction.md
[implementation-ruby]: ../../languages/ruby/exercises/concept/strings/.docs/introduction.md
[implementation-python]: ../../languages/python/exercises/concept/strings/.docs/introduction.md
[implementation-go]: ../../languages/go/exercises/concept/basic-strings/.docs/introduction.md
