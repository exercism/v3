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

The last expression in a chain of `&&` and `||` does not need to be a boolean expression.
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
