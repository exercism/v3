## General

[using statement][using-statement] documentation describes how and when to use the `using` keyword.

## 1. Write to the database

Consider the various methods on the database, and the states that the database transitions to with each one.

## 2. Write to the database and return an indication of whether the write was successful to the caller

Use of [`try/catch`][try-catch] is necessary in this case.

[using-statement]: https://docs.microsoft.com/en-us/dotnet/csharp/language-reference/keywords/using-statement
[try-catch]: https://docs.microsoft.com/en-us/dotnet/csharp/language-reference/keywords/try-catch
