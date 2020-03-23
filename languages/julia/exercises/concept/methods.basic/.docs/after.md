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

### Defining functions

There are two common ways to define a named function in Julia:

```julia
function preptime(layers)
    2 * layers
end
```

and

```julia
preptime(layers) = 2 * layers
```

The latter is most commonly used for one-line function definitions or mathematical functions.

### Implicit multiplication

Julia supports implicit multiplication to allow code that looks similar to maths notation:

```julia
julia> f(x) = 3x

julia> f(3)
9
```

### Naming

Julia code often uses "germaniccase" for short function and variable names, e.g. `preptime`, `isbits`, or `eigvals`, while longer names use "snake_case", e.g. `total_working_time`.
The line when to use which is a bit blurry.
Some Julia style guides suggest using snake_case everywhere, e.g. the [Blue Style][blue-style], but Julia's standard and Base libraries prefer germaniccase.
Note that you should not use "camelCase" for variables or functions, as that is reserved for type names, which we will get to later.

[blue-style]: https://github.com/invenia/BlueStyle
