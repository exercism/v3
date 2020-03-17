In F#, a _value_ is everything that has a type and can be defined. That includes booleans, integers and lists, and also functions. Integer values are defined as one or more (consecutive) digits and support the [default mathematical operators][operators].

Assigning a value to a name is referred to as a _binding_. As [bindings][bindings] are immutable, this makes bindings similar to constants in other languages. Bindings are defined using the `let` keyword and have three components: a name, value and type.

[Functions][functions] are also regular bindings, but with one or more parameters, which are separated by spaces. A function will automatically return its last expression.

Invoking a function is done by specifying its name and passing arguments for each of the function's parameters, again separated by spaces.

Specifying the type of a binding is optional in most cases. If no type is specified for a binding, the F# compiler will use _type inference_ to try to automatically infer the type of the binding based on its value. Type inference also works for most functions, by analyzing what values the function is called with and what value the function returns.

If a binding's type cannot be inferred, the compiler will report an error. To fix this, make the binding's type explicit.

F# uses significant whitespace to define scope. This means that scope is defined by indenting the code with spaces, relative to the line declaring the binding. The default convention is to use four spaces for indentation:

[bindings]: https://docs.microsoft.com/en-us/dotnet/fsharp/language-reference/values/#binding-a-value
[functions]: https://docs.microsoft.com/en-us/dotnet/fsharp/language-reference/functions/#remarks
[operators]: https://docs.microsoft.com/en-us/dotnet/fsharp/language-reference/symbol-and-operator-reference/arithmetic-operators
