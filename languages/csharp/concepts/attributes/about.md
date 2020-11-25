A [C# `Attribute`][attribute-concept] provides a way to decorate a declaration to associate metadata to: a class, a method, an enum, a field, a property or any [other supported][attribute-targets] declarations.

You can apply [an attribute][attribute] to a declaration by adding it on the line before the declaration using a `ClassAttribute` and a `FieldAttribute`:

```csharp
[Class]
class MyClass
{
    [Field]
    int myField;
}
```

The declarative metadata only associates additional structured information to the code and does not modify its behavior, but that metadata is used by other part of the code to change how its target would behave or add, change or remove, restrict some its functionalities.

Multiple predefined attributes exist like: `Flags`, `Obsolete`, `Conditional`. Note that the full name of the [attribute `Flags`][flags-attribute] is `FlagsAttribute` by convention, but suffix Attribute can be omitted when used in the code.

Short presentation of four predefined attributes:

- `[Flags]`: Predefined in the System namespace. Indicates the enum to supports bitwise operations and the method `Enum.HasFlag()`, `ToString` displays all the flags [see example][flags-example].
- `[Obsolete]`: Predefined in the System namespace. Allows to add a message about why the code is obsolete, it can be used to display compiler warnings or error.
- `[Conditional]`: Predefined in the System.Diagnostics namespace. Allows to remove some method calls at compile time for debugging (diagnostics) purposes.
- `[CallerMemberName]`: Predefined in the System.Runtime.CompilerServices namespace. Allows you to obtain information about the caller to a method.

[attribute-concept]: https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/concepts/attributes/
[attribute]: https://docs.microsoft.com/en-us/dotnet/csharp/tutorials/attributes
[attribute-targets]: https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/concepts/attributes/#attribute-targets
[flags-attribute]: https://docs.microsoft.com/en-us/dotnet/api/system.flagsattribute?view=net-5.0
[flags-example]: https://docs.microsoft.com/en-us/dotnet/api/system.flagsattribute?view=net-5.0#examples
