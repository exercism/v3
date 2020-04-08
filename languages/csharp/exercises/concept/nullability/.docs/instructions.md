In this exercise you'll be writing code to helping printing name
badges for factory employees.

Employees have an id, a name and a department name. Badge labels are
formatted as follows: `"[id] - [name] - [DEPARTMENT]"`

### 1. Generate the badge label from employee properties

Writing a static method called `Badge.Label()` to compute the string
to be printed on the badge:

```csharp
Badge.Label(734, "Ernest Johnny Payne", "Strategic Communication");
// => "[734] - Ernest Johnny Payne - STRATEGIC COMMUNICATION"
```

The department name is optional. If someone has no department, just
print `GUEST` as their department name.

### 2. Word wrap the label text so it fits on a badge

Writing a static method called `Badge.PrintLabel()`:

```csharp
Badge.PrintLabel("[734] - Ernest Johnny Payne - STRATEGIC COMMUNICATION", 30);
// => "[734] - Ernest Johnny Payne - \nSTRATEGIC COMMUNICATION"
```

The width is optional, so if it is not provided, just return the
string as is.
