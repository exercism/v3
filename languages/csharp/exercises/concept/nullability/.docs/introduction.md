Up to now, we saw how to declare variables and how to assign them to
values.  For example, the following code snippet declares a variable
`a` of type `int` whose value is `5`:

```
int a = 5;
```

Sometimes, we need to indicate that a variable has no specific value.s
In C#, we can use the `null` literal to denote the abscence of a
value.  A *nullable* type is a type that allow `null` values.  The
operator `?` can be used to indicate that a type is nullable.

In the following example, we declare a variable `b` of type `int?`
whose value is `5`:

```csharp
int? b = 5;
```

`b` is nullable while `a` is not. That means that `a` should always
contain an integer value, while `b` can be emptied:

```charp
a = null; // invalid
b = null; // valid!
```

Null values are a convenience, but it comes with a price. A common
error is trying to manipulate `null` as they were actual values.  For
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
