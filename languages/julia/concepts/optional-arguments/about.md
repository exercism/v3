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
