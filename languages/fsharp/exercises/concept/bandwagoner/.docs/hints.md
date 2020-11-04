### 1. Define the model

- [This page][define] shows how to define a discriminated union.

### 2. Create a team's coach

- [This page][create] shows how to create an instance of a record.

### 3. Create a team's stats

- [This page][create] shows how to create an instance of a record.

### 4. Create a team

- [This page][create] shows how to create an instance of a record.

### 5. Replace the coach

- There is [special syntax][create] to return a copy of a record but with one or more fields having a new value.

### 6. Check for same team

- Records have built-in [structural equality][equality], which means that records that have the same values are equal.

### 7. Check if you should root for a team

- The best way to execute logic based on the team's value is to use [pattern matching][pattern-matching].
- The pattern to match on records is through the [record pattern][record-patterns].
- If you want to add an additional condition to a pattern, you can add a [guard][guards].
- The [record pattern][record-patterns] can also be applied to nested records.

[define]: https://docs.microsoft.com/en-us/dotnet/fsharp/language-reference/records#remarks
[create]: https://docs.microsoft.com/en-us/dotnet/fsharp/language-reference/records#creating-records-by-using-record-expressions
[equality]: https://docs.microsoft.com/en-us/dotnet/fsharp/language-reference/records#differences-between-records-and-classes
[pattern-matching]: https://docs.microsoft.com/en-us/dotnet/fsharp/language-reference/pattern-matching
[record-patterns]: https://docs.microsoft.com/en-us/dotnet/fsharp/language-reference/pattern-matching#record-pattern
[guards]: https://docs.microsoft.com/en-us/dotnet/fsharp/language-reference/match-expressions#guards-on-patterns
