The ability for something to be defined in terms of itself is called recursion. In F#, recursion is most commonly found in [recursive functions][recursive-functions], which are functions that call themselves. A recursive function is defined like a regular function, but with the `rec` modifier. Without this modifier, a function will _not_ be able to call itself any any attempt to do so will result in a compilation error. Recursion thus has to be explicitly opted into.

```fsharp
let rec factorial x =
    if x = 1 then
        1
    else
        x * factorial (x - 1)
```

One possible issue with recursive functions is that each (recursive) function call results in allocations on the stack, causing it to grow. As the stack has a limited amount of memory, functions that use many recursive calls may cause a `StackOverflowException` to be thrown. How many recursive calls will trigger this behavior depends on multiple factors, like the available memory on the stack and how much data each function call takes up on the stack.

The best way to prevent recursive functions from overflowing the stack is to write them as _tail-recursive_ functions. When a function is [tail-recursive][tail-recursion], the compiler can optimize its output such that there are no additional allocations on the stack for each recursive function call. The one rule for tail-recursive functions is that the result of a recursive function call should be directly returned from the function, there must not be any additional code that processes the result of the recursive call. The most common way to do this is by creating a helper function that takes an additional accumulator argument, which is updated for each recursive call and returned when the recursion finishes. [This page][tail-recursion-in-depth] goes into great detail on how the compiler processed tail-recursive functions.

Here is an example of a function that sums the values in a list in a tail-recursive way:

```fsharp
let rec sumRecursive acc remainder =
    match remainder with
    | [] -> acc
    | x::xs -> sumRecursive (acc + x) xs

let sum list = sumRecursive 0 list

sum [1; 2; 3]
// => 6
```

It is quite common to define the recursive helper as a nested function, which hides the recursive nature of the function from the caller:

```fsharp
let sum list =
    let rec sumRecursive acc remainder =
        match remainder with
        | [] -> acc
        | x::xs -> sumRecursive (acc + x) xs

    sumRecursive 0 list
```

F# supports [recursive types through discriminated union types][recursive-types]. A recursive discriminated union type has one or more of its cases refer to the discriminated union type itself in their data.

```fsharp
type RussianDoll
    | Doll
    | Layer of RussianDoll
```

Note that the `rec` keyword is not used to define recursive types.

[recursive-functions]: https://docs.microsoft.com/en-us/dotnet/fsharp/language-reference/functions/recursive-functions-the-rec-keyword
[recursive-types]: https://fsharpforfunandprofit.com/posts/recursive-types-and-folds/#a-basic-recursive-type
[tail-recursion]: https://cyanbyfuchsia.wordpress.com/2014/02/12/recursion-and-tail-recursion-in-f/
[tail-recursion-in-depth]: https://devblogs.microsoft.com/fsharpteam/tail-calls-in-f/
