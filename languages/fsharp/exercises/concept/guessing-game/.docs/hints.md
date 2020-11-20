## General

- [This page][pattern-matching] has a nice introduction to pattern matching in F#.

## 1. Reply to a correct guess

- You can use a [constant pattern][constant-patterns] to match on a specific number.

## 2. Reply to a close guess

- You can either use a [constant pattern][constant-patterns] or a [variable pattern][variable-patterns] and a [guard][guards].

## 3. Reply to too low guesses

- You can use a combination of a [variable pattern][variable-patterns] and a [guard][guards].

## 4. Reply to too high guesses

- You can use a combination of a [variable pattern][variable-patterns] and a [guard][guards].

[pattern-matching]: https://docs.microsoft.com/en-us/dotnet/fsharp/language-reference/pattern-matching
[constant-patterns]: https://docs.microsoft.com/en-us/dotnet/fsharp/language-reference/pattern-matching#constant-patterns
[wildcard-patterns]: https://docs.microsoft.com/en-us/dotnet/fsharp/language-reference/pattern-matching#wildcard-pattern
[variable-patterns]: https://docs.microsoft.com/en-us/dotnet/fsharp/language-reference/pattern-matching#variable-patterns
[guards]: https://docs.microsoft.com/en-us/dotnet/fsharp/language-reference/match-expressions#guards-on-patterns
[exhaustive-matching]: https://fsharpforfunandprofit.com/posts/match-expression/#exhaustive-matching
