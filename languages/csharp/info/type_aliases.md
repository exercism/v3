# Type aliases

C# runs on the .NET framework, which has a set of built-in types you can use from C#. To make using these BCL types easier, C# has defined type aliases for the most common BCL types:

| C# type   | .NET type        |
| --------- | ---------------- |
| `bool`    | `System.Boolean` |
| `byte`    | `System.Byte`    |
| `sbyte`   | `System.SByte`   |
| `char`    | `System.Char`    |
| `decimal` | `System.Decimal` |
| `double`  | `System.Double`  |
| `float`   | `System.Single`  |
| `int`     | `System.Int32`   |
| `uint`    | `System.UInt32`  |
| `long`    | `System.Int64`   |
| `ulong`   | `System.UInt64`  |
| `object`  | `System.Object`  |
| `short`   | `System.Int16`   |
| `ushort`  | `System.UInt16`  |
| `string`  | `System.String`  |

For more information, see the [built-in types table][docs.microsoft.com_built-in-types-table].

## Which type to use?

The general consensus is to prefer the C# type over the .NET type. Here are some examples where this prefererence shows:

- The default [C# editorconfig settings][docs.microsoft.com_editorconfig-language-keywords].
- The [ReSharper built-in type naming rule][jetbrains.com_built-in-type-naming].
- The [CoreFX coding style document][github.com_corefx-coding-style].

[docs.microsoft.com_built-in-types-table]: https://docs.microsoft.com/en-us/dotnet/csharp/language-reference/keywords/built-in-types-table
[docs.microsoft.com_editorconfig-language-keywords]: https://docs.microsoft.com/en-us/visualstudio/ide/editorconfig-language-conventions?view=vs-2019#language-keywords
[github.com_corefx-coding-style]: https://github.com/dotnet/corefx/blob/master/Documentation/coding-guidelines/coding-style.md
[jetbrains.com_built-in-type-naming]: https://www.jetbrains.com/help/resharper/Built_In_Type_Naming.html
