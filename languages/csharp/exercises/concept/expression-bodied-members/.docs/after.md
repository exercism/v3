Many [types of struct and class members][expression-bodied-members] (fields being the primary exception) can use the expression-bodied member syntax. Defining members through expression bodies often produces more concise and readable code than traditional blocks/statements.

```csharp
int Times3(int input) => input * 3;

int Interesting => 1729;
```

Expression-bodied-members cannot have blocks of multiple statements, but those with a functional background should be warned that anything can be achieved by one of these members as a traditional member. The "expression" can be an assignment operation creating side effects, or a method invocation meaning that anything is possible.

#### Ternary Operators

Ternary operators allow if-conditions to be defined in expressions rather than statement blocks. This echoes functional programming approaches and can often make code more expressive and less error-prone.

```csharp
int a = 3, b = 4;
int max = a > b ? a : b;
```

#### Throw expressions

`throw` expressions are an alternative to `throw` statements and are particularly useful when combined with ternary expressions.

```csharp
string trimmed = str == null ? throw new ArgumentException() : str.Trim();
```

#### Reference

- [Expressions][expressions]: explains what expressions are.
- [Expression-bodied members][expression-bodied-members]: what expression-bodied members are and how to define them.

[expressions]: https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/statements-expressions-operators/expressions
[expression-bodied-members]: https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/statements-expressions-operators/expression-bodied-members
