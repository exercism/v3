In this exercise you'll be writing code to helping printing name
badges for factory employees.

Employees have an ID, name and department name. Badge labels are
formatted as follows: `"[id] - [name] - [DEPARTMENT]"`

### 1. Generate an employee's badge label

Implement the `Badge.Label()` method to return an employee's badge label:

```csharp
Badge.Label(734, "Ernest Johnny Payne", "Strategic Communication");
// => "[734] - Ernest Johnny Payne - STRATEGIC COMMUNICATION"
```

### 2. Deal with the optional parameters on the badge labels

The ID and department name are optional. If someone does not have an
ID, do not print this part of the label.  If someone has no
department, just print `GUEST` as their department name.

### 2. Convert the label text to fit on a badge with a given width

Implement the `Badge.PrintLabel()` method to return an employee's
badge label that can fit on a badge with a given width:

```csharp
Badge.PrintLabel("[734] - Ernest Johnny Payne - STRATEGIC COMMUNICATION", 30);
// => "[734] - Ernest Johnny Payne - \nSTRATEGIC COMMUNICATION"
```
