Sometimes, we need to indicate that a variable has no value.  In C#,
we can use the `null` literal to denote the absence of a value.  A
*nullable* type is a type that allows `null` values.  The operator `?`
can be added as a suffix to a type to make it nullable.

In the following example, we declare a regular, non-nullable variable
`a` of type `int` and a nullable variable `b` of type `int?` whose
value is `5`:

```csharp
int a = 4;
int? b = 5;
```

`a` must contain an integer value, while `b` can also be set to
`null`:

```csharp
a = null; // Compile error as a is not nullable
a = 5;    // Valid

b = null; // Valid
b = 6;    // Valid
```

A common mistake is trying to call a method on variable that has been
set to `null`.  What happens is that the C# runtime raises an
exception called `NullReferenceException` in this situation.

If you try to compile the following code:

```csharp
string? userName = ...;

...

Console.WriteLine(userName.Length);
```

The compiler will give the following warning message:

```
Dereference of a possibly null reference. (CS8602)
```

A good practice is testing if a nullable value is not `null` before
trying to do something with it. There are some cases however, where we
know for sure that, given the context, a variable cannot be `null`.

In order to dismiss the warning, we can use the operator `!`:

```csharp
Console.WriteLine(userName!.Length);
```

Finally, sometimes, we are want to provide a default value to a
variable, in case it is `null`. You can use a `if` or a `?:` operator
to do that. However, as you can imagine, this can become cumbersome
the more nullable variables you have on your code.  

The `??` operator is a simple shortcut to that:

```csharp
Console.WriteLine((userName ?? "").Length);
```
