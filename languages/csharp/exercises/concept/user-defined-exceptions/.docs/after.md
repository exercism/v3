A user-defined exception is any class defined in your code that is derived from `System.Exception`. It is subject to all the rules of class inheritance but in addition the compiler and language runtime treat such classes in a special way allowing their instances to be thrown and caught outside the normal control flow as discussed in the `exceptions` exercise. User-defined exceptions can be used in every way like runtime and Microsoft Base Class Library exceptions.

This special treatment applies only to `Exception`-derived classes. You cannot throw instances of any other type.

User-defined exceptions are often used to carry extra information such as a message and other relevant data to be made available to the catching routines. This can then be recorded in logs, reported to a user or perhaps used in a retry operation. `System.Exception` has convenient data members and appropriate constructors to hold a message and the "inner" exception.

Whilst using user-defined exceptionss to wrap and enhance third party exceptions is a frequently seen pattern, the general advice is not to use them outside of this use case too liberally in your own code. It is considered an anti-pattern. There are challenges to this view and you can see both sides of the argument in this [Stack Exchange post][se-exceptions].

This [article][create-user-defined-exceptons] is a good introduction to user-defined exceptions.

## Exception Filters

The following articles are of general interest:

- [Exception filters][exception-filters]: how to filter user-defined-exceptions.

talk about not making exceptions part of the regular control flow.
should be reserved for circumstances that can't be oredicted in advance
such as IO errors,

[create-user-defined-exceptions]: https://docs.microsoft.com/en-us/dotnet/standard/exceptions/how-to-create-user-defined-exceptions
[exception-filters]: https://docs.microsoft.com/en-us/dotnet/csharp/language-reference/keywords/when
[se-udfs]: https://softwareengineering.stackexchange.com/questions/189222/are-exceptions-as-control-flow-considered-a-serious-antipattern-if-so-why
