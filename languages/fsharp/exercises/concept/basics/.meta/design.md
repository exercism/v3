## Learning objectives

- Know what a value is.
- Know how to define a binding.
- Know how to define a function.
- Know how to return a value from a function.
- Know how to call a function.
- Know that bindings are immutable.
- Know how type inference works for bindings.
- Know how to define scope using significant whitespace.
- Know that bindings require dependency order.
- Know how to define an integer.
- Know how to use mathematical operators on integers.
- Know how to define single- and multiline comments.

## Out of scope

- Generic values.
- Mutable values.
- Reference cells.
- Naming rules for identifiers.
- Double-quoted identifiers.
- Shadowing.
- Memory and performance characteristics.
- Returning functions.
- Recursive functions.
- Higher-order functions.
- Anonymous functions
- Inline functions.
- Pure functions.
- Partial application.
- Currying.
- Organizing functions in modules and namespaces.
- Visibility.

## Concepts

- `basics`: know what a value is; know how to define a binding; know that bindings are immutable; know how to define scope using significant whitespace; know that bindings require dependency order; know how to define a function; know how to return a value from a function; know how to call a function; know what type inference is; know how type inference works for bindings; know how to define an integer; know how to use mathematical operators on integers; Know how to define single- and multiline comments.

## Prequisites

There are no prerequisites.

## Analyzer

This exercise could benefit from the following rules added to the the [analyzer][analyzer]:

- Verify that the whitespace adheres to community defaults.
- Verify that the `remainingMinutesInOven` function uses the `expectedMinutesInOven` binding.
- Verify that the `elapsedTimeInMinutes` function actually calls the `preparationTimeInMinutes` function.

[analyzer]: https://github.com/exercism/fsharp-analyzer
[values]: https://docs.microsoft.com/en-us/dotnet/fsharp/language-reference/values/
