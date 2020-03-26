In this exercise, we'll learn how to declare and detect `null` variables. 
We will also see how the C# can help us dealing with `null` values and variables.

### 1. Detect null values

Write a static method called `IsNull` to detect if a string is `null` or not.

``` csharp
Nullable.IsNull("hello") // should return `false`
Nullable.IsNull(null)    // should return `true`
```

### 2. Letting the runtime detect null values

The C# runtime is able to detect when code calls a method from a `null` value. 
It throws a `NullReferenceException`.

Write a static method called `TotalLength` to sum the lengths of two strings passed as parameters.
It should throw `NullReferenceException` when one of the strings is `null`.

``` csharp
Nullable.TotalLength("abc", "def"); // returns `6`
Nullable.TotalLength(null, "def");  // throws a `NullReferenceException`
```

### 3. Dealing with null values

Write a static method called `TotalLengthSmart` to sum the lengths of two strings passed as parameters.
If one of the passed strings is `null`, use `0` as its length.

``` csharp
Nullable.TotalLengthSmart("abc", "def"); // returns `6`
Nullable.TotalLengthSmart(null, "def");  // returns `3`
```

**BONUS:** Try to write this method without using `if`s.

### 4. Nullable reference types

You may have noticed this warning when you compile your code or run your tests.

Fix it.

`Cannot convert null literal to non-nullable reference type.`

### 5. TODO: null forgiving operator

