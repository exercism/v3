The ability for something to be defined in terms of itself is called recursion. In F#, recursion is most commonly found in recursive functions, which are functions that call themselves. A recursive function is defined like a regular function, but with the `rec` modifier. Without this modifier, a function will _not_ be able to call itself and any attempt to do so will result in a compilation error. Recursion thus has to be explicitly opted into.

```fsharp
let rec factorial x =
    if x = 1 then
        1
    else
        x * factorial (x - 1)
```

F# also supports recursive types through discriminated union types. A recursive discriminated union type has one or more of its cases refer to the discriminated union type itself in their data.

```fsharp
type RussianDoll
    | Doll
    | Layer of RussianDoll
```

Note that the `rec` keyword is not used to define recursive types.

Lists in F# can be considered as a recursive type, with a list being either an empty list or having a head element followed by a list. Pattern matching can be used to process a list recursively:

```fsharp
let rec length list =
    match list with
    | [] -> 0
    | head::tail -> 1 + length tail
```
