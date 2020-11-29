Simple `if-else` statements can also be written using the ternary operator `a ? b : c`.
If `a` is true, then `b` will be evaluated.
Otherwise evaluate `c`.

For example the following function

```julia
julia> function diagnose(heartrate)
           if heartrate > 100
               println("Elevated heart rate.")
           else
               println("Nominal heart rate or dead.")
           end
       end
diagnose (generic function with 1 method)
```

can also be written as

```julia
julia> diagnose(heartrate) = heartrate > 100 ? println("Elevated heart rate.") : println("Nominal heart rate or dead.")
diagnose (generic function with 1 method)
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
