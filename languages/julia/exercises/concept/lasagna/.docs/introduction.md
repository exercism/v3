The entire Julia track will require you to treat your solution like small libraries, i.e. you need to define functions, types etc. which will then be run against a test suite.
For that reason, we will introduce named functions as the very first concept.

<!-- TODO: Add a note that explains the REPL julia block syntax-->

## Defining functions

There are two common ways to define a named function in Julia:

```julia
julia> function muladd(x, y, z)
           return x * y + z
       end
muladd (generic function with 1 method)
```

and

```julia
julia> muladd(x, y, z) = x * y + z
muladd (generic function with 1 method)
```

The latter is most commonly used for one-line function definitions or mathematical functions.

## Invoking functions

Invoking a function is done by specifying its name and passing arguments for each of the function's parameters:

```julia
julia> muladd(10, 5, 1)
51
```

And of course you can invoke a function within the body of another function:

```julia
julia> square_plus_one(x) = muladd(x, x, 1)
square_plus_one (generic function with 1 method)
```

## Types

Depending on which other programming languages you know, you may expect parameters, variables or return values to have explicit type annotations.
For now, assume that that Julia will infer the types automagically and don't worry about them, we will get to the specifics of the type system in later exercises.

## Comments

Julia supports two kinds of comments.
Single line comments are preceded by `#` and multiline comments are inserted between `#=` and `=#`.

```julia
julia> 2 + 2 # returns 4
4
```

```julia
julia> #= Some random code that's no longer needed but not deleted
       sub(x, y) = x - y
       mulsub(x, y, z) = sub(mul(x, y), z)
       =#

```
