Given a year, report if it is a leap year in the Gregorian calendar.

## Arithmetic Operators: Remainder & Equality

Julia provides a number of [arithmetic operators](https://en.wikipedia.org/wiki/Arithmetic#Arithmetic_operations) for primitive numeric types[^1].

In this exercise you will only need the `%` operator that calculates the [remainder](https://en.wikipedia.org/wiki/Remainder)

```julia
julia> 10 % 5
0

julia> 8 % 5
3
```

and the equality operator `==`

```julia
julia> 2 + 2 == 4
true

julia> 2 + 2 == 5
false
```

<!-- TODO Check if admonitions will be supported -->

!!! note

    It is conventional to have one space either side of each operator:

    ```julia
    x % y == 3
    ```

    instead of

    ```julia
    x%y==3
    ```

[^1]: You can find a list of them in the [Julia Manual](https://docs.julialang.org/en/v1/manual/mathematical-operations/#Arithmetic-Operators).
