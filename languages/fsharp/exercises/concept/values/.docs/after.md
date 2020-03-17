In F#, a _value_ is everything that has a type and can be defined. That includes booleans, integers and lists, but also functions. Integer values are defined as one or more (consecutive) digits and support the [default mathematical operators][operators].

Assigning a value to a name is referred to as a _binding_. [Bindings][bindings] are immutable, which makes them similar to constants in other languages. Bindings are defined using the `let` keyword.

As F# is a statically-typed language, each binding has a type. Specifying a binding's type is optional for most bindings, as F#'s _type inference_ can usually infer the type based on their value.

[Functions][functions] are also regular bindings, but with one or more parameters. A function automatically returns its last expression. Type inference also works for most functions, by analyzing what values the function is called with and what value the function returns.

Invoking a function is done by specifying its name and passing arguments for each of the function's parameters.

If a binding's type cannot be inferred, the compiler will report an error. To fix this, make the binding's type explicit.

F# uses significant whitespace to define scope. This means that scope is defined by indenting the code with spaces, relative to the line declaring the binding. The default convention is to use four spaces for indentation.

[bindings]: https://docs.microsoft.com/en-us/dotnet/fsharp/language-reference/values/#binding-a-value
[functions]: https://docs.microsoft.com/en-us/dotnet/fsharp/language-reference/functions/#remarks
[operators]: https://docs.microsoft.com/en-us/dotnet/fsharp/language-reference/symbol-and-operator-reference/arithmetic-operators
