Up to now, we saw how to declare variables and how to assign them to
values.  For example, the following code snippet declares a variable
`a` of type `int` whose value is `5`:

```
int a = 5;
```

Sometimes, we need to indicate that a variable has no value.
In C#, we can use the `null` literal to denote the abscence of a
value.  A *nullable* type is a type that allows `null` values.  The
operator `?` can be added as a suffix to a type to make it nullable.

In the following example, we declare a regular, non-nullable variable `a` of type `int` and a nullable variable `b` of type `int?`
whose value is `5`:

```csharp
int? b = 5;
```

`b` is nullable while `a` is not. That means that `a` must
contain an integer value, while `b` can also be set to `null`:

```charp
a = null; // Compile error as a is not nullable
b = null; // Valid
b = 6;    // Valid
```

Null values are a convenience, but it comes with a price. A common
error is trying to call a member on variable that has been set to `null`.   For
example, what if someone tries to print the length of a variable
containing an empty user name like that:


```csharp
string? userName = null;

Console.WriteLine(userName.Length);
```

What happens is that the C# runtime raises an exception called
`NullReferenceException` in this situation.

The C# compiler also tries to help us, if you try to compile this
code, you will see the following warning message:

```
Dereference of a possibly null reference. (CS8602)
```

A good practice, is testing if a variable is empty before trying to do
something with it. In order to do that, we compare its value with
`null`:

```csharp
if (userName != null)
{
    Console.WriteLine(userName.Length);
}
```

In some cases, we know for sure that, given the context, a variable
cannot be `null`.  In order to dismiss the warning, we can use the
operator `!`:

```csharp
Console.WriteLine(userName!.Length);
```

Finally, sometimes, we are want to provide a default value to a
variable, it case it is `null`. A simple way to that, would be with a
`if` or a `?:` operator:

```csharp
if (userName == null)
{
    userName = "";
}

Console.WriteLine(userName.Length);
```

```csharp
Console.WriteLine((userName == null ? "" : userName).Length);
```


As you can imagine, this can become cumbersome the more nullable variables you have on your code.
The `??` operator is a simple shortcut to that:

```csharp
Console.WriteLine((userName ?? "").Length);
```
