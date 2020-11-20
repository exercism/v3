### Summary

This exercise showed how to:

- Define functions that take one or more arguments and return a value.
- Do arithmetic calculations with integers.
- Invoke functions.

#### Defining functions

There are two common ways to define a named function in Julia:

```julia
function muladd(x, y, z)
    return x * y + z
end
```

and

```julia
muladd(x, y, z) = x * y + z
```

The latter is most commonly used for one-line function definitions or mathematical functions.

#### Invoking functions

Invoking a function is done by specifying its name and passing arguments for each of the function's parameters:

```julia
add(x, y) = x + y
mul(x, y) = x * y
muladd(x, y, z) = add(mul(x, y), z)
```

#### Comments

Julia supports two kinds of comments.
Single line comments are preceded by `#` and multiline comments are inserted between `#=` and `=#`.

```julia
add(1, 3) # returns 4

#= Some random code that's no longer needed but not deleted
sub(x, y) = x - y
mulsub(x, y, z) = sub(mul(x, y), z)
=#
```

### Further details

For more information about functions, consider taking a look at the [Julia Manual][functions]. Note that the concepts after the "The `return` Keyword" section will be introduced in future exercises.

#### Implicit returns

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

#### Naming

Julia code often uses "germaniccase" for short function and variable names, e.g. `preptime`, `isbits`, or `eigvals`, while longer names use "snake_case", e.g. `total_working_time`.
The line when to use which is a bit blurry.
Some Julia style guides suggest using snake_case for all function and variable names, e.g. the [Blue Style][blue-style], but Julia's standard and Base libraries prefer germaniccase.
Julia programs generally do not use "lowerCamelCase" anywhere and reserve "UpperCamelCase" for type names, which we will get to later.

[blue-style]: https://github.com/invenia/BlueStyle
[functions]: https://docs.julialang.org/en/v1/manual/functions/
