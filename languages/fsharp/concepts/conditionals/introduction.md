In this exercise you must conditionally execute logic. The most common way to do this in F# is by using an `if/elif/else` statement:

```fsharp
if x = 5 then
    // Expression to evaluate when x equals 5
elif x > 7 then
    // Expression to evaluate when x greater than 7
else
    // Expression to evaluate in all other cases
```

The condition(s) used in an `if/elif/else` expression must be of type `bool`. F# has no concept of _truthy_ values.
