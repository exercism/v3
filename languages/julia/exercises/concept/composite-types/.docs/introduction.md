This exercise introduces abstract and composite types, probably the most common custom types that you will write or use.

The Julia manual [Types chapter][manual] is a great reference for this (and well written!), but in brief:

> Abstract types cannot be instantiated, and serve only as nodes in the type graph, thereby describing sets of related concrete types: those concrete types which are their descendants.

Define them like so:

```julia
abstract type Number end
abstract type Real <: Number end
```

You can read these lines as "`Number` is an abstract type" and "`Real` is an abstract subtype of `Number`".

> Composite types are called records, structs, or objects in various languages. A composite type is a collection of named fields, an instance of which can be treated as a single value. In many languages, composite types are the only kind of user-definable type, and they are by far the most commonly used user-defined type in Julia as well.

You can define them like this:

```julia
struct Point
    x
    y
end
```

Read that as "`Point` is a composite type with fields `x` and `y`".

You can construct an _instance_ of Point with `Point(10, 5)`

If you like, you can specify a supertype and/or the types of the fields:

```julia
abstract type AbstractPoint end

struct Point <: AbstractPoint
    x::Int
    y::Int
end
```

If you omit a supertype, the implicit supertype is `Any`.

You can define parameterised (generic) types, too, but we'll leave that to the manual.

[manual]: https://docs.julialang.org/en/v1/manual/types/
