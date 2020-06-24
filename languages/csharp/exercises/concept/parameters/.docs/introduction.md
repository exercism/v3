This exercise discusses some details of method parameters and their use in C#.

The behavior of parameters can be modified by the use of modifies such as `ref` and `out`.

- Parameters passed without a modifier (such as `out` or `ref`) are passed by value. That is to say that the parameter as seen in the calling method cannot be changed by the called method.
- A parameter with the `out` modifier conveys a value back from the called method to the caller. The parameter can be passed to the called method without being initialized and in any case it is treated within the called method as if, on entry, it had not been initialized. An understanding of the behavior and rules regarding parameter modifiers can be gained most easily through examples (see below) and compiler messages.

```csharp
void Foo(out int val)
{
    val = 42;
}
void Bar(int val)
{
    val = 42;
}

int importantValue = 1729;
Foo(out importantValue);
var result = importantValue == 42;
// => true

importantValue = 1729;
Bar(inportantValue);
result = importantValue == 1729;
// => true
```

- `out` parameters must be assigned to within a called method.

- A parameter with the `ref` modifier passes a value into a called method. When the method returns the caller will find any updates made by the called method in that parameter.

```csharp
void Foo(ref int val)
{
    val *= 7;
}

int importantValue = 6;
Foo(ref importantValue);
return importantValue;
// => 42

```

- `ref` parameters must be variables as the called method will be operating directly on the parameter as seen by the caller.
- The `out` and `ref` modifiers are required both in the called method signature and at the call site.
- `out` parameters can be declared in line at the call site viz: `Foo(out int importantValue);`.
- If you make a call to a method which has `out` parameters but you are not interested in the value assigned to one or more of them then you can use the discard dummy variable `_`, as in: `Foo(out int _);`.
