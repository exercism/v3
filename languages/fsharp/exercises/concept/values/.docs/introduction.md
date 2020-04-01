In F#, assigning a value to a name is referred to as a _binding_. Bindings are immutable, which makes them similar to constants in other languages. As F# is a statically-typed language, each binding has a type.

Bindings are defined using the `let` keyword. Specifying a binding's type is optional for most bindings, as F#'s _type inference_ can usually infer the type based on their value.

```fsharp
let fingers = 10
```

Functions are also regular bindings, but with one or more parameters, which are separated by spaces. A function will automatically return its last expression. Type inference also works for most functions, by analyzing what values the function is called with and what value the function returns.

```fsharp
let add x y = x + y
```

Invoking a function is done by specifying its name and passing arguments for each of the function's parameters, again separated by spaces.

```fsharp
let five = add 2 3
```

If a binding's type cannot be inferred, the compiler will report an error. To fix this, make the binding's type explicit.

```fsharp
let fingers: int = 10

let add (x: int) (y: int): int = x + y
```

F# uses significant whitespace to define scope. This means that scope is defined by indenting the code with spaces, relative to the line declaring the binding. The default convention is to use four spaces for indentation:

```fsharp
let toes =
    let left = 5
    let right = 5
    left + right

let multiplyPlusTwo x y =
    let product = x * y
    product + 2
```
