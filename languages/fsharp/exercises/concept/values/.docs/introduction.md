In F#, a _value_ is everything that has a type and can be defined. That includes booleans, integers and lists, but also functions. As F# is a _functional-first_ programming language, functions are at the heart of the language.

Associating a name with a value is referred to as a _binding_. A binding is defined using the `let` keyword. Function bindings are regular bindings, but with one or more parameters, which are separated by spaces. Invoking a function is done by specifying its name and passing arguments for each of the function's parameters, again separated by spaces.

A key aspect of bindings is that they're immutable. This means that once a binding has been defined, its value can never change. As a consequence, a new binding has to be defined for changing values.

Specifing the type of a binding is optional. If no type is specified for a binding, the F# compiler will use _type inference_ to try to automatically infer the type of the binding. It will do this based on the value of the binding. For functions, the type is inferred by analyzing what values the function is called with and what value the function returns.

F# uses significant whitespace to define scope. This means that scope is defined by indenting the code with spaces, relative to the line declaring the binding. The default convention is to use four spaces for indentation.
