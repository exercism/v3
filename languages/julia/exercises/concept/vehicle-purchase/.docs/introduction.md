There are three primary conditional statements that are used in Julia:
- `if`-statements
- The ternary operator
- Short-circuit evaluation

## `if`-statements

`if`-statements in Julia are similar to those seen in other languages:

```julia
julia> function ispositive(n)
           if n > 0
               println("n is positive!")
           else
               println("n is not positive!")
           end
       end
ispositive (generic function with 1 method)

julia> ispositive(10)
n is positive!

julia> ispositive(-10)
n is not positive!
```

<!-- TODO: Add that fancy concept highlight embed thing to boolean expression -->
If the boolean expression following the `if` evalutes to `true`, the first block of code is run and the second block is skipped.
If the boolean expression following the `if` evalutes to `false`, the first block of code is skipped and the second block is run.
The program continues running at the first line of code after the `end` keyword.

!!! info
    In Julia, the `end` keyword signifies the end of all block expressions.
    This syntax is not specific to `if`-statements or function definitions.

In cases where the second block of code would be just another `if`-statement, `elseif` allows us to avoid nesting `if`-statements within the block:

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

If an `if`-statement only needs to perform code for one of the cases, there's no need to write out the `else` branch.

## Ternary operator

Simple `if-else` statements can also be written using the ternary operator `a ? b : c`.
If `a` is true, then `b` will be evaluated.
Otherwise evaluate `c`.

For example the following function

```julia
julia> function diagnose(heartrate)
           if heartrate > 100
               println("The patient has an elevated heart rate.")
           else
               println("The heart rate is nominal or the patient is dead.")
           end
       end
diagnose (generic function with 1 method)
```

can also be written as

```julia
julia> diagnose(heartrate) = heartrate > 100 ? println("The patient has an elevated heart rate.") : println("The heart rate is nominal or the patient is dead.")diagnose (generic function with 1 method)
```

This is especially useful if you want to assign a different value to a variable depending on a condition.
`x = a ? b : c` will assign `b` to `x` if `a` is `true` and otherwise assign `c` to `x`.
For example:

```julia
julia> function patient_status(heartrate)
           status = heartrate > 0 ? "alive" : "dead"

           println("The patient is ", status)
       end

julia> patient_status(100)
The patient is alive

julia> patient_status(0)
The patient is dead
```

## Short-Circuit Evaluation

If several boolean expressions are chained together using the `&&` and `||` operators, Julia will only evaluate the least amount of expressions that are necessary to determine the value of the entire chain.

For an example, consider the expression chain `n > 0 && k > 10`.
If `n` equals `5`, the first part of the chain, `n > 0`, is `true` and it's necessary to evaluate the second expression `k > 10` to determine if the entire expression is `true`.
However, if `n` equals `-5`, we know that the chain as a whole **cannot** be `true` because its first expression is `false`.

If we consider `n > 0 || k > 10` instead, the opposite happens.
If `n` equals `5`, the first part of the chain, `n > 0`, is `true` and we know that the whole chain will be `true` regardless of the second expression.
However, if `n` equals `-5`, it's necessary to evaluate the second expression.

This behaviour can be used to write very short `if` statements:

```julia
if a
    b
end
```

is equivalent to

```julia
a && b
```

You will often find this feature being used for early errors or returns.
For example, using the `dessert(fruit)` function from above:

```julia
julia> function whats_for_dessert(fruit, allergic_to)
           fruit == allergic_to && return "I can't eat that, guess I'll not have dessert tonight!"

           return dessert(fruit)
       end
whats_for_dinner (generic function with 1 method)

julia> whats_for_dinner("apple", "peach")
"Apple Crumble"

julia> whats_for_dinner("apple", "apple")
"I can't eat that, guess I'll not have dessert tonight!"
```

<!-- TODO: Mention ifelse(cond, x, y) somewhere as an infobox -->
