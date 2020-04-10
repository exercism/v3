### General

The following reference documentation may help you finishing this exercice:

- [Null keyword][null-keyword]: reference documentation for `null` keyword.
- [Nullable types tutorial][nullable-types-tutorial]: basic tutorial on how to work with nullable types.
- [Nullable reference types][nullable-reference-types]: how to use nullable reference types.
- [Null-coalescing operator][null-coalescing-operator]: explains how the null-coalescing operator works.
- [Null-conditional operator][null-conditional-operator]: explains how the null-conditional operator works.
- [Nullable reference types tutorial][nullable-reference-types-tutorial]: tutorial on nullable reference types.

# 1. Badge.Label

* Do not forget to convert the department to upper case if it is not `nulll`;
* Do not forget to add `GUEST` as the department name if there's no
   provided department name.

# 2. Badge.PrintLabel with a maxWidth

* Which method from the `string` class may help you to extract a
   [substring][https://docs.microsoft.com/en-us/dotnet/api/system.string.substring?view=netframework-4.8]
   of a string?
* Do all slices always have the same length? If not, how to handle
   this case?

# 2. Badge.PrintLabel without maxWidth

* You can use an `if` to check if `maxWidth` is null before
   trying to split the string.

[null-keyword]: https://docs.microsoft.com/en-us/dotnet/csharp/language-reference/keywords/null
[nullable-types-tutorial]: https://csharp.net-tutorials.com/data-types/nullable-types/
[null-coalescing-operator]: https://docs.microsoft.com/en-us/dotnet/csharp/language-reference/operators/null-coalescing-operator
[null-conditional-operator]: https://docs.microsoft.com/en-us/dotnet/csharp/language-reference/operators/conditional-operator
[nullable-reference-types]: https://docs.microsoft.com/en-us/dotnet/csharp/nullable-references
[nullable-reference-types-tutorial]: https://docs.microsoft.com/en-us/archive/msdn-magazine/2018/february/essential-net-csharp-8-0-and-nullable-reference-types
[substring]: https://docs.microsoft.com/en-us/dotnet/api/system.string.substring?view=netframework-4.8
