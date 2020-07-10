The [discriminated union][define] type represents a fixed number of named cases. Each value of a discriminated union corresponds to exactly one of the named cases. This type of data type is known as a _sum type_.

Each case of a discriminated union can optionally have data associated with it, and different cases can have different types of data. If none of the cases have data associated with them, the discriminated union is similar to what other languages usually refer to as an _enumeration_ (or _enum_).

A discriminated union is defined using the `type` keyword and requires very little syntax. This makes them easy to use, and you'll find them used in many places, including the base library.

```fsharp
// Discriminated union without associated data
type Season =
    | Spring
    | Summer
    | Autumn
    | Winter

// Discriminated union with associated data
type Number =
    | Integer of int
    | Float of float
    | Invalid
```

Creating a value for a specific case can be done by referring to its name, or its full name if the name is not unique. As case names are just constructor functions, associated data can be passed as a regular function argument.

```fsharp
let byName = Integer 2
let byFullName = Number.Invalid
```

Discriminated unions have _structural equality_, which means that two values for the same case and with the same (optional) data are equivalent.

The preferred way to work with discriminated unions is through [pattern matching][pattern-matching] using the [identifier pattern][identifier-patterns]:

```fsharp
let describe number =
    match number with
    | Integer i -> sprintf "Integer: %d" i
    | Float d  -> sprintf "Float: %d" i
    | Invalid   -> "Invalid"
```

[define]: https://docs.microsoft.com/en-us/dotnet/fsharp/language-reference/discriminated-unions#remarks
[pattern-matching]: https://docs.microsoft.com/en-us/dotnet/fsharp/language-reference/pattern-matching
[constant-patterns]: https://docs.microsoft.com/en-us/dotnet/fsharp/language-reference/pattern-matching#constant-patterns
[identifier-patterns]: https://docs.microsoft.com/en-us/dotnet/fsharp/language-reference/pattern-matching#identifier-patterns
[wildcard-patterns]: https://docs.microsoft.com/en-us/dotnet/fsharp/language-reference/pattern-matching#wildcard-pattern
[guards]: https://docs.microsoft.com/en-us/dotnet/fsharp/language-reference/match-expressions#guards-on-patterns
[exhaustive-matching]: https://fsharpforfunandprofit.com/posts/match-expression/#exhaustive-matching
