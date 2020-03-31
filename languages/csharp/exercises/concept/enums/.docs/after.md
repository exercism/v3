You can use an enum whenever you have a fixed set of constant values. Using an enum gives one a type-safe way of interacting with constant values. Defining an enum is done through the `enum` keyword. An enum member is referred to by prepending it with the enum name and a dot (e.g. `Status.Active`).

Each enum member has an associated integer value associated with it. If no value is explicitly defined for an enum member, its value is automatically assigned to `1` plus + the previous member's value. If the first member does not have an explicit value, its value is set to `0`.

Another benefit of enums are that they are very declarative. Compare the following two pieces of code:

```csharp
Users.WithStatus(1)
```

vs

```csharp
Users.WithStatus(Status.Active)
```

For someone reading the code, the second (enum) version will be easier to comprehend.

You should always consider using an enum whenever you want to model something as a boolean. Besides the aforementioned readability benefits, enums have another advantage over booleans: new values can always be added to an enum, whereas a boolean value will only ever be `true` or `false`. Using an enum is thus more future proof.

Note that while one _can_ cast integer values to an enum, it is also possible to cast from an integer value that doesn't map to any enum value:

```csharp
public enum Status
{
    Inactive = 0,
    Active = 1
}

Status status = (Status) 2;
status == Status.Inactive; // False
status == Status.Active;   // False
```
