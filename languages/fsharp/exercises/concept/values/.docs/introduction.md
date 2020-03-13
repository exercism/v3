In F#, a _value_ is everything that has a type and can be defined. That includes booleans, integers, strings, lists, but also functions.

Associating a name with a value is referred to as a _binding_. A binding is defined using the `let` keyword. The key aspect of bindings is that they're immutable. This means that once a binding has been defined, its value can never change. As a consequence, a new binding has to be defined for changing values.

F# does support mutable bindings, which are known as _variables_. To define a variable, add the `mutable` modifier to the binding. As immutability is the default in F#, variables are used sparingly; the default is regular, immutable bindings.

One final, interesting aspect of bindings is that the F# compiler supports _type inference_. Therefore, specifying a binding's type is optional, as the compiler can infer its type from the value it is associated with.
