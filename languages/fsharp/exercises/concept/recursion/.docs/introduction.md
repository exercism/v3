The ability for something to be defined in terms of itself is called recursion. In F#, recursion is most commonly found in recursive functions, which are functions that call themselves.

A recursive function needs to have at least one _base case_ and at least one _recursive case_. A _base case_ returns a value without calling the function again. A _recursive case_ calls the function again, modifying the input so that it will at some point match the base case.

Recursive functions are defined like regular functions, but with the `rec` modifier. Without this modifier, a function will _not_ be able to call itself and any attempt to do so will result in a compilation error. Recursion thus has to be explicitly opted into.

```fsharp
let rec factorial x =
    // Base case
    if x = 1 then
        1
    // Recursive case
    else
        x * factorial (x - 1)
```

F# also supports recursive types through discriminated union types. A recursive discriminated union type has one or more of its cases refer to the discriminated union type itself in their data. Like recursive functions, recursive types must have a base case. Unlike recursive functions, recursive types don't use the `rec` keyword.

```fsharp
type RussianDoll
    | Doll                 // Base case
    | Layer of RussianDoll // Recursive case
```
