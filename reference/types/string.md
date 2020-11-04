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

### High School Sweethearts

This exercise customizes an ASCII heart with initials of given two full names. The reference implementation (Elixir) teaches:

- String concatenation
- String interpolation
- Trimming
- Changing casing (uppercase)
- Extracting substrings (first letter)
- Multiline strings

#### Implementations

| Track  | Exercise                         | Changes |
| ------ | -------------------------------- | ------- |
| Elixir | [strings][implementation-elixir] | None    |

### Log Lines

This exercise extracts information from log lines. The reference implementation (C#) teaches:

- String concatenation
- Trimming
- Changing casing
- Extracting substrings

#### Implementations

| Track  | Exercise                                 | Changes |
| ------ | ---------------------------------------- | ------- |
| C++    | [strings][implementation-cpp]            | None    |
| C#     | [log-levels][implementation-csharp]      | None    |
| F#     | [strings][implementation-fsharp]         | None    |
| Ruby   | [strings][implementation-ruby]           | None    |
| Python | [strings][implementation-python]         | None    |
| Go     | [strings-package][implementation-go-pkg] | None    |

### Poetry Club

This exercise is a conversation between you and the security guard. The reference implementation (JavaScript) teaches:

- String substrings (first, last letter)
- String casing (Word capitalization, normalisation).
- String reversing
- String concatenation

#### Implementations

| Track      | Exercise                             | Changes |
| ---------- | ------------------------------------ | ------- |
| JavaScript | [strings][implementation-javascript] | None    |

### Party Robot

This exercise implements some phrases an eccentric party robot would say.

- String introduction
- String formatting

#### Implementations

| Track | Exercise                     | Changes |
| ----- | ---------------------------- | ------- |
| Go    | [strings][implementation-go] | None    |

[type-char]: ./char.md
[implementation-cpp]: ../../languages/cpp/exercises/concept/strings/.docs/introduction.md
[implementation-csharp]: ../../languages/csharp/exercises/concept/log-levels/.docs/introduction.md
[implementation-elixir]: ../../languages/elixir/exercises/concept/high-school-sweetheart/.docs/introduction.md
[implementation-fsharp]: ../../languages/fsharp/exercises/concept/log-levels/.docs/introduction.md
[implementation-ruby]: ../../languages/ruby/exercises/concept/strings/.docs/introduction.md
[implementation-python]: ../../languages/python/exercises/concept/processing-logs/.docs/introduction.md
[implementation-go-pkg]: ../../languages/go/exercises/concept/strings-package/.docs/instructions.md
[implementation-go]: ../../languages/go/exercises/concept/strings/.docs/instructions.md
[implementation-javascript]: ../../languages/javascript/exercises/concept/strings/.docs/
