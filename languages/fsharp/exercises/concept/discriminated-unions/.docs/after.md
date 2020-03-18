The [discriminated union][define] type represents a fixed number of named cases. Each value of a discriminated union corresponds to exactly one of the named cases. This type of data type is known as a _sum type_.

Each case of a discriminated union can optionally have data associated with it, and different cases can have different types of data. If none of the cases have data associated with them, the discriminated union is similar to what other languages usually refer to as an _enumeration_ (or _enum_).

A discriminated union is defined using the `type` keyword and requires very little syntax. This makes them easy to use, and you'll find them used in many places, including the base library.

Creating a value for a specific case can be done by referring to its name, or its full name if the name is not unique. As case names are just constructor functions, associated data can be passed as a regular function argument.

Discriminated unions have _structural equality_, which means that two values for the same case and with the same (optional) data are equivalent.

The preferred way to work with discriminated unions is through [pattern matching][pattern-matching]. Pattern matching is a way in which a value can be tested against one or more _patterns_. There are different types of patterns, including [constant patterns][constant-patterns], [identifier patterns][identifier-patterns] (which are used to match discriminated union cases) and [wildcard patterns][wildcard-patterns]. Pattern matching is similar to switch statements in other languages, although most switch statements only support constant patterns.

A match expression is defined using the `match` keyword. The value is tested against each pattern from top to bottom, until it finds one that matches and then execute the logic associated with the pattern. Additional conditions, known as [_guards_][guards], can be added to patterns using the `when` keyword.

The F# compiler will output a warning if it detects that a `match` expression does not handle _all_ possible cases. This is known as [_exhaustive matching_][exhaustive-matching].

[define]: https://docs.microsoft.com/en-us/dotnet/fsharp/language-reference/discriminated-unions#remarks
[pattern-matching]: https://docs.microsoft.com/en-us/dotnet/fsharp/language-reference/pattern-matching
[constant-patterns]: https://docs.microsoft.com/en-us/dotnet/fsharp/language-reference/pattern-matching#constant-patterns
[identifier-patterns]: https://docs.microsoft.com/en-us/dotnet/fsharp/language-reference/pattern-matching#identifier-patterns
[wildcard-patterns]: https://docs.microsoft.com/en-us/dotnet/fsharp/language-reference/pattern-matching#wildcard-pattern
[guards]: https://docs.microsoft.com/en-us/dotnet/fsharp/language-reference/match-expressions#guards-on-patterns
[exhaustive-matching]: https://fsharpforfunandprofit.com/posts/match-expression/#exhaustive-matching
