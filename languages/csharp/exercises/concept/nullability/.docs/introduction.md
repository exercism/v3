In C# and in many other languages, the `null` literal denotes the abscence of a value.

A *nullable* type is a type that allow `null` values.

There are two main families of types in C#: *reference* and *value*
types. A variable of a *value* type, contains a direct value in
memory, while a *reference* typed variable contains a *pointer* to a
value.

A variable has a *nullable* type if it can contain no value. *Reference* types are nullable by default. 
*Value* types can be made nullable by means of the operator *?*.
