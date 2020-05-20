### 1. Define the approval

- [This page][define] shows how to define a discriminated union.

### 4. Define the activity

- [This page][define] shows how to define a discriminated union, both for cases with and without associated data.

### 5. Rate the activity

- The best way to execute logic based on the activity's value is to use [pattern matching][pattern-matching].
- The pattern to match discriminated union cases (and optionally, their associated data) is through [identifier patterns][identifier-patterns].
- If you want to add an additional condition to a pattern, you can add a [guard][guards].
- If you want to catch more than one value at once, you can use a [wildcard pattern][wildcard-pattern].

[define]: https://docs.microsoft.com/en-us/dotnet/fsharp/language-reference/discriminated-unions#remarks
[pattern-matching]: https://docs.microsoft.com/en-us/dotnet/fsharp/language-reference/pattern-matching
[identifier-patterns]: https://docs.microsoft.com/en-us/dotnet/fsharp/language-reference/pattern-matching#identifier-patterns
[wildcard-patterns]: https://docs.microsoft.com/en-us/dotnet/fsharp/language-reference/pattern-matching#wildcard-pattern
[guards]: https://docs.microsoft.com/en-us/dotnet/fsharp/language-reference/match-expressions#guards-on-patterns
