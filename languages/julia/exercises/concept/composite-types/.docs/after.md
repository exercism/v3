### Summary

This exercise showed:

- How to define an abstract type.
- How to define a subtype of an abstract type.
- How to define a composite type with an inner constructor.
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

#### Type hierarchy

#### Inner constructors

Inner constructors can be used to enforce invariants.

For more information about inner constructors, consider taking a look at the [Julia Manual][inner-constructors].

### Further details

[inner-constructors]: https://docs.julialang.org/en/v1/manual/constructors/#Inner-Constructor-Methods-1
