### Summary

This exercise showed:

- When to use inner constructors.

### Examples solution

```julia
abstract type Chesspiece end

struct Pawn <: Chesspiece
    colour::Symbol

    function Pawn(colour)
        if colour in (:black, :white)
            new(colour)
        else
            throw(DomainError(colour, "colour must be :black or :white"))
        end
    end
end

colour(p::Pawn) = p.colour
```

#### Inner constructors

Inner constructors can be used to enforce invariants.

For more information about inner constructors, consider taking a look at the [Julia Manual][inner-constructors].

#### 

### Further details


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
Some Julia style guides suggest using snake_case everywhere, e.g. the [Blue Style][blue-style], but Julia's standard and Base libraries prefer germaniccase.
Note that you should not use "UpperCamelCase" or "camelCase" for variables or functions, as that is reserved for type names, which we will get to later.

[inner-constructors]: https://docs.julialang.org/en/v1/manual/constructors/#Inner-Constructor-Methods-1
