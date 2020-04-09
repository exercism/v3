In this exercise you'll be writing code to helping printing name
badges for factory employees.

Employees have an id, name and department name. Badge labels are
formatted as follows: `"[id] - [name] - [DEPARTMENT]"`

### 1. Generate an employee's badge label

Implement the `Badge.Label()` method to return an employee's badge label:

```csharp
Badge.Label(734, "Ernest Johnny Payne", "Strategic Communication");
// => "[734] - Ernest Johnny Payne - STRATEGIC COMMUNICATION"
```

The department name is optional. If someone has no department, just
print `GUEST` as their department name.

### 2. Convert the label text to fit on a badge with a given width

Implement the `Badge.PrintLabel()` method to return an employee's badge label that can fit on a badge with a given width:

```csharp
Badge.PrintLabel("[734] - Ernest Johnny Payne - STRATEGIC COMMUNICATION", 30);
// => "[734] - Ernest Johnny Payne - \nSTRATEGIC COMMUNICATION"
```

The width is optional, so if it is not provided, just return the
string as is.
