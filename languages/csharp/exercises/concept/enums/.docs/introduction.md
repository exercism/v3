# Introduction

The C# `enum` type represents a fixed set of named constants (an enumeration). Its chief purpose is to provide a type-safe way of interacting with numeric constants, limiting the available values to a pre-defined set.

Using `enums` is a great way to limit the possible values of a variable. It allows you to create your own variable type, e.g. `int`, `char`, and `bool`, and **define its set of allowable values**. You'd have to define the `enum` first with its possible values then, you can use the defined `enum` to give your variables that type. That variable can now only have possible values as defined by that `enum`.

For example, you're working with months, you would want to create an ```enum``` called _Month_, and set its allowable values to the 12 possible months. Then you can create a variable of type ```Month``` and name it _firstMonth_. The compiler, or your tool will only allow you to set ```firstMonth``` to one of the possible values. Here's an example:

```csharp
public enum Month
{
    January,
    February,
    // The rest of the months here
    December
}

public class EnumConversionExample
{
    public static void Main()
    {
        Month firstMonth = Month.January; // First month of the year is January
        Console.WriteLine(firstMonth); // Prints out `January`

        firstMonth = "Hello World!"; // Results in a compilation error!
    }
}
```

This is mostly used to help us read and write our code. It gets rid of _magic numbers_, wherein you assign a number to a status (much like error 404). It helps in removing the cognitive load of having to translate the number to something that's meaningful. Aside from that, your tool would also offer you auto-complete to the possible values, very useful if you've got values that's too long to type.

## Sources

[Enums Conversion](https://docs.microsoft.com/en-us/dotnet/csharp/language-reference/builtin-types/enum#conversions)

[W3Schools](https://www.w3schools.com/cs/cs_enums.asp)
