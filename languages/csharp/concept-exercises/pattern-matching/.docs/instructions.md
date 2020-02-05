# Instructions

In this exercise you'll use pattern matching to check types input into
your method as well as determine the perimeter of the shape's input.

### 1. Check the type 

First, take a look at the Shapes file to determine which shape objects
you'll need to support.

```csharp
public static class Square {}
```

### 2. Pattern match the types  

Implement a method that checks the types and then returns the right perimeter using:
1. Switches
2. If Then
3. C# 8 style pattern matching

```csharp
switch (shape)
{
    case Square s:
    break;
}

if (shape is Square s)
{
}

var perimeter = shape switch
{
}
```

### 3. Implement perimeter math 

Find out perimeter for the different shapes.

```csharp
Square s => s.Side * 4,
```

### 4. Check for edge cases 

What if the user doesn't supply a correct object, or no input?

```csharp
public static double Perimeter(object shape = null)
```
