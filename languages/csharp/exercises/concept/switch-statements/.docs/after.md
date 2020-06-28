Switch statements have a venerable [history][wiki-switch] in programming languages. They were introduced in `C` where they were prized for their speed. That speed came at the cost of functionality which was very constrained. In C# the role of the switch statement has been expanded beyond integers. Switch statements can encompass any arbitrary type, primitive or reference.

If you are coming from a functional language then working with switch statements (and [switch expressions][switch-expression] discussed elsewhere) is the nearest you will get in C# to using discriminated unions and pattern matching. However, they have no where near the discriminated union's power to enforce type safety.

Simple switch statements resemble their `C` ancestors combining `switch`, `case`, `break` and `default`.

```csharp
int direction = GetDirection();
switch (direction)
{
    case 1:
        GoLeft();
        break;
    case 2:
        GoRight();
        break;
    default:
        MarkTime();
        break;
}
```

The above pattern can be used with any simple (primitives + strings) type.

When reference types are added into the mix then extra syntax is involved, firstly to down cast the type and then to add guards (`when`) although guards can be used with simple value cases. This is illustrated below:

```csharp
Animal animal = GetAnimal();

switch(animal)
{
    case Dog dog:
        dog.Bark();
        break;
    case Cat cat when cat.Had8Lives():
        cat.IsCareful();
        cat.Meow();
        break;
    case Cat cat:
        cat.Meow();
        break;
}
```

- The `default` clause is optional but typically desirable.
- The `break` statement is mandatory for any non-empty `case` clause.
- Obviously the type of all the arguments to the `case` statements must be derived from the type of the `switch` argument. A `switch` argument of type `Object` obviously allows the widest range.
- The guard expression can include anything in scope not just members of the `case` argument.

[switch statement][switch-statement] documentation provides an introduction to `switch` statements.

[wiki-switch]: https://en.wikipedia.org/wiki/Switch_statement
[switch-statement]: https://docs.microsoft.com/en-us/dotnet/csharp/language-reference/keywords/switch
[switch-expression]: https://docs.microsoft.com/en-us/dotnet/csharp/language-reference/operators/switch-expression#basic-example
