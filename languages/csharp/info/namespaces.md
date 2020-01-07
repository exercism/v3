# Namespaces

Namespaces in C# have two primary functions:

1. Organizing classes.
1. Control scoping of classes.

## Types in a namespace

Types are uniquely identified by their name in combination with their namespace. This is referred to as their _full name_, defined as: `<namespace>.<type>`.

By default, types within the same namespace can be referred to by just their name. To refer to types in a different namespace, one has to either:

- Use the type's full name.
- Import the type's namespace through the [`using` keyword][csharp-keyword-using].

[csharp-keyword-using]: ../keywords/using.md

## Resources

- See [this page][docs.microsoft.com-namespaces] for more information on namespaces.

[docs.microsoft.com-namespaces]: https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/namespaces/
