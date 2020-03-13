In F#, a _value_ is everything that has a type and can be defined. That includes booleans, integers, strings, lists, but also functions.

A [_binding_][bindings] is used to associate a name with a value. To define a binding, the `let` keyword is used, followed by the binding's name and value.

The key aspect of bindings is that they are immutable. This means that once a binding has been defined, its value can never change. As a consequence, if you want a new value, you need a new binding.

F# also support mutable bindings, known as [_variables_][variables]. To define a variable, add the `mutable` modifier to the binding. Updating a variable is done using the `<-` operator, as opposed to the `=` operator to define a binding. The separate update operator, combined with the `mutable` modifier, makes it very explicit when F# code deals with mutation.

As immutability is the default in F#, variables are used sparingly; the default is regular, immutable bindings. There are several [benefits to using immutability][immutability], such as the code being easier to reason about and enabling the compiler to better reason about the code.

Lastly, the F# compiler supports _type inference_. Therefore, specifying a binding's type is optional, as the compiler can infer its type from the value it is associated with.

[bindings]: https://docs.microsoft.com/en-us/dotnet/fsharp/language-reference/values/#binding-a-value
[variables]: https://docs.microsoft.com/en-us/dotnet/fsharp/language-reference/values/#mutable-variables
[immutability]: https://docs.microsoft.com/en-us/dotnet/fsharp/language-reference/values/#why-immutable
