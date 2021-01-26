`if`-expressions in Julia are similar to those seen in other languages:

```julia
julia> function say_if_positive(n)
           if n > 0
               println("n is positive!")
           else
               println("n is not positive!")
           end
       end
say_if_positive (generic function with 1 method)

julia> say_if_positive(10)
n is positive!

julia> say_if_positive(-10)
n is not positive!
```

<!-- TODO: Add that fancy concept highlight embed thing to boolean expression -->

If the boolean expression following the `if` evaluates to `true`, the first block of code is run and the second block is skipped.
If the boolean expression following the `if` evaluates to `false`, the first block of code is skipped and the second block is run.
The program continues running at the first line of code after the `end` keyword.

<!-- prettier-ignore -->
!!! info
    In Julia, the `end` keyword signifies the end of all block expressions.
    This syntax is not specific to `if`-expressions or function definitions.

In cases where the second block of code would be just another `if`-expression, `elseif` allows us to avoid nesting `if`-expressions within the block:

```julia
julia> function dessert(fruit)
           if fruit == "apple"
               return "Apple Crumble"
           elseif fruit == "lemon"
               return "Lemon Meringue Pie"
           else
               return "Fruit Salad"
           end
       end
dessert (generic function with 1 method)

julia> dessert("apple")
"Apple Crumble"

julia> dessert("lemon")
"Lemon Meringue Pie"

julia> dessert("peach")
"Fruit Salad"
```

If an `if`-expression only needs to perform code for one of the cases, there's no need to write out the `else` branch.
