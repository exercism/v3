An `if/elif/else` expression can be used to conditionally execute logic. F# also has another, more powerful way to conditionally execute logic: pattern matching. With pattern matching, a value can be tested against one or more _patterns_. An example of such a pattern is the _constant pattern_, which matches a value against a constant (e.g. `1` or `"hello"`).

In F#, pattern matching is done through the `match` keyword:

```fsharp
let describe number =
    match number with
    | 0 -> "Zero"
    | 1 -> "One"
```

While this may look like `switch` statements in other languages, pattern matching starts to shine when also using other patterns. One such pattern is the _variable pattern_, which allows one to capture a value:

```fsharp
match number with
| 0 -> "Zero"
| i -> sprintf: "Non zero: %d" i
```

In some cases, you may want to add an additional condition to a pattern. This is known as a _guard_ (clause), which can be added using the `when` keyword:

```fsharp
match number with
| 0 -> "Zero"
| i when i < 0 -> "Negative number"
```

In the above example, not all possible input will have a matching pattern. The compiler will detect this and output a warning. This is known as _exhaustive checking_. To solve the warning, one has to handle all cases. For this, the _wildcard pattern_ can be used, which is a pattern that matches on any value:

```fsharp
match number with
| i when i < 0 -> "Negative number"
| _ -> "Positive number"

// No compiler warning
```

Pattern matching will test a value against each pattern from top to bottom, until it finds a matching pattern and executes the logic associated with that pattern. The order of patterns matters!
