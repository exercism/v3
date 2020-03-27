### General

The following reference documentation may help you finishing this exercice:

- [Null keyword][null-keyword]: reference documentation for `null` keyword.
- [Nullable types tutorial][nullable-types-tutorial]: basic tutorial on how to work with nullable types.
- [Nullable reference types][nullable-reference-types]: how to use nullable reference types.
- [Null-coalescing operator][null-coalescing-operator]: explains how the null-coalescing operator works.
- [Null-conditional operator][null-conditional-operator]: explains how the null-conditional operator works.
- [Nullable reference types tutorial][nullable-reference-types-tutorial]: tutorial on nullable reference types.

[null-keyword]: https://docs.microsoft.com/en-us/dotnet/csharp/language-reference/keywords/null
[nullable-types-tutorial]: https://csharp.net-tutorials.com/data-types/nullable-types/
[null-coalescing-operator]: https://docs.microsoft.com/en-us/dotnet/csharp/language-reference/operators/null-coalescing-operator
[null-conditional-operator]: https://docs.microsoft.com/en-us/dotnet/csharp/language-reference/operators/conditional-operator
[nullable-reference-types]: https://docs.microsoft.com/en-us/dotnet/csharp/nullable-references
[nullable-reference-types-tutorial]: https://docs.microsoft.com/en-us/archive/msdn-magazine/2018/february/essential-net-csharp-8-0-and-nullable-reference-types

# 1. Compute the width of each word separately

For each word:

* Determine if it as an actual string or `null`;
* Determine if there is an actual font size, or `null`;
* Multiply the length of the string by the font size or `0`.


# 2. Compute the number of spaces to be displayed between words

Add the number of words multiplied by the font size to the final width.
