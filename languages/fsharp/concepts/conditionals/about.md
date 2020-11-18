Conditionally executing code can be done using [`if/elif/else` expressions][conditional-expression]. The condition(s) used in an `if/elif/else` expression must be of type `bool`. F# has no concept of _truthy_ values.

```fsharp
if x = 5 then
    printfn "x equals 5"
elif x > 7 then
    printfn "x is not 5 and greater than 7"
else
    printfn "x is not 5 and not greater than 7"
```

As `if/elif/else` is an expression, it can be bound to a name or returned from a function.

```fsharp
let message =
    if x = 5 then
        "x equals 5"
    elif x > 7 then
        "x is not 5 and greater than 7"
    else
        "x is not 5 and not greater than 7"
```

All branches must return the same type. Not doing so results in a compiler error:

```fsharp
if x = 5 then
    true
elif x > 7 then
    5.6

// Results in compiler erropr
```

The `else` and `elif` branches are optional, but only if the type of the returned expression is [`unit`][unit-type].

```fsharp
// Valid if expression
if x > 2 then
    printfn "x is greater than two"

// Invalid if expression, results in compiler warning
if x > 2 then
    true
```

An `if/else` expression can also be placed on a single line. This should only be used for simple `if` expressions.

```fsharp
let y = if x > 5 then 10 else 20
```

[unit-type]: https://docs.microsoft.com/en-us/dotnet/fsharp/language-reference/unit-type
[conditional-expression]: https://docs.microsoft.com/en-us/dotnet/fsharp/language-reference/conditional-expressions-if-then-else
