The entire Julia track will require you to treat your solution like a mini-libraries, i.e. you need to define functions, types etc. which will then be run against a test suite.
For that reason, we will introduce named functions as the very first concept.

### Defining functions

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

### Invoking functions

Invoking a function is done by specifying its name and passing arguments for each of the function's parameters:

```julia
add(x, y) = x + y
mul(x, y) = x * y
muladd(x, y, z) = add(mul(x, y), z)
```

### Types

Depending on which other programming languages you know, you may expect parameters, variables or return values to have explicit type annotations.
For now, assume that that Julia will figure out the types automagically and don't worry about them, we will get to the specifics of the type system in later exercises.

### Comments

Julia supports two kinds of comments.
Single line comments are preceded by `#` and multiline comments are inserted between `#=` and `=#`.

```julia
add(1, 3) # returns 4

#= Some random code that's no longer needed but not deleted
sub(x, y) = x - y
mulsub(x, y, z) = sub(mul(x, y), z)
=#
```
