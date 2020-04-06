### Implicit returns

The `return` statement is optional in Julia.
The last value in the function body will be returned implicitely:

```julia
function preptime(layers)
    return 2 * layers
end
```

and

```julia
function preptime(layers)
    2 * layers
end
```

are equivalent.

### Naming

Julia code often uses "germaniccase" for short function and variable names, e.g. `preptime`, `isbits`, or `eigvals`, while longer names use "snake_case", e.g. `total_working_time`.
The line when to use which is a bit blurry.
Some Julia style guides suggest using snake_case everywhere, e.g. the [Blue Style][blue-style], but Julia's standard and Base libraries prefer germaniccase.
Note that you should not use "camelCase" for variables or functions, as that is reserved for type names, which we will get to later.

[blue-style]: https://github.com/invenia/BlueStyle
