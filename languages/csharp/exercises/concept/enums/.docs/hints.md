### General

- [Tutorial on working with enums][docs.microsoft.com-enumeration-types].

### 1. Parse log level

- There is a method to get a substring within a string, [Substring()](https://docs.microsoft.com/en-us/dotnet/api/system.string.substring?view=netframework-4.8)
- You can use a [switch-case statement](https://docs.microsoft.com/en-us/dotnet/csharp/language-reference/keywords/switch) to map each log level code to an enum.

### 2. Support unknown log level

- There's a [default](https://docs.microsoft.com/en-us/dotnet/csharp/language-reference/keywords/switch#the-default-case) statement to catch all unspecified cases.

### 3. Convert log line to short format

- Converting an enum to a number can be done through [casting][docs.microsoft.com_enumeration-types-casting] or by using a [format string][docs.microsoft.com_system.enum.tostring].

[docs.microsoft.com-enumeration-types]: https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/enumeration-types
[docs.microsoft.com_enumeration-types-casting]: https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/enumeration-types#code-try-1
