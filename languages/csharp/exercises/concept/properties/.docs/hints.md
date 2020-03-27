### General

- [Properties][docs.microsoft.com-properties]
- [Using Properties][docs.microsoft.com-using-properties]

### 1. Allow the weight to be set on the weighing machine 

A property with a private [backing field](https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/classes-and-structs/properties#properties-with-backing-fields) is appropriate here.

### 2. Ensure that a negative input weight is rejected.

Add [validation](https://stackoverflow.com/questions/4946227/validating-properties-in-c-sharp) to the `InputWeight`'s `set` accessor to throw an exception.

### 3. Allow the US weight to be retrieved

A property can return a reference to an object for example:

### 4. Allow the machine's units to be set to pounds

`Units` is a good candidate for an [auto-implemented property](https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/classes-and-structs/auto-implemented-properties).

### 5. Allow a tare adjustment to be applied to the weighing machine

Accessors can have [different access levels](https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/classes-and-structs/restricting-accessor-accessibility) to each other.


[docs.microsoft.com-properties]: https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/classes-and-structs/properties
[docs.microsoft.com-using-properties]: https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/classes-and-structs/using-properties
