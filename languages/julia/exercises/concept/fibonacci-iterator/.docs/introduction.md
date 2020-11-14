## iterator-protocol

<!-- TODO: Motivate why iterators are useful -->

To implement the informal iteration interface, you need to define two methods `iterate(iter::MyIter)` and `iterate(iter::MyIter, state)`.
Both methods must return a tuple of an item and the state of the iterator.
The first method will return the first item and state, while the second method will return the next item and state.
The iteration ends if `iterate` returns `nothing`.

!!! note
    Julia currently doesn't have a way to formally define interfaces.
    To implement an interface, you need to look up which methods need to be defined in the Manual, or in the documentation of the package that "defines" the interface.
    Alternatively, you can work your way through `MethodErrors` to find out which methods you need to implement:
    ```julia
    julia> struct MyIter end;

    julia> for i in MyIter()
            @show i
        end
    ERROR: MethodError: no method matching iterate(::MyIter)
    ...
    ```

!!! note
    Julia iterators track the state decoupled from the iterator object.
    The iterator object itself is generally not mutated.

To make it clearer when the `iterate` methods are called, one can look at the translation of the `for`-loop syntax:

```julia
for item in iter
    println(item)
end
```

will be translated to

```julia
next_item = iterate(iter)

while !isnothing(next_item)
    (item, state) = next_item
    
    println(item)

    next_item = iterate(iter, state)
end
```

### Example

We want to define an iterator `Squares(n)` to iterate the sequence of [square numbers](https://en.wikipedia.org/wiki/Square_number) smaller than `n`. Square numbers are numbers that are the square of an integer. For example, 9 is a square number, since it can be written as `3 * 3`.  The end result should look like:

```julia
julia> for i in Squares(20)
           println(i)
       end
1
4
9
16
```

To achieve that, we define `iterate(iter)` as follows

```julia
Base.iterate(S::Squares) = (1^2, 2)
```

and `iterate(iter, state)` as

```julia
function Base.iterate(S::Squares, state)
    item = state^2

    if item > S.max
        return nothing
    end

    (item, state + 1)
end
```

You may find it useful to combine these two definitions into one using optional arguments:

### optional-arguments

```julia
julia> f(a, b=1) = a + b
f (generic function with 2 methods)
```

is equivalent to

```julia
julia> f(a, b) = a + b
f (generic function with 1 method)

julia> f(a) = f(a, 1)
f (generic function with 2 methods)
```

In the response in the REPL, you can see that the first definition defines two methods at once.

This is particularly useful when the computation of the next item is identical regardless if it's the first or a consecutive iteration.
Some iterators may require a more extensive initialisation, in which case it can be better to split them into two separate definitions.

### Source

This entire section is based on the Julia Manual section on the [Iteration Interface](https://docs.julialang.org/en/v1/manual/interfaces/#man-interface-iteration)[^1].

[^1]: accessed November 10, 2020
