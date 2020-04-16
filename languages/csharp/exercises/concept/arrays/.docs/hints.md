### General

- The bird count is stored as a [field][fields] named `counts`.
- The bird count array contains exactly 7 integers.

### 1. Calculate the total number of visiting birds

- A variable can be used to hold the total number of visiting birds.
- The array can be iterated over using a [`foreach` loop][array-foreach].
- The variable can be updated inside the loop.

### 2. Calculate the number of busy days

- A variable can be used to hold the number of busy days.
- The array can be iterated over using a [`foreach` loop][array-foreach].
- The variable can be updated inside the loop.
- A [conditional statement][if-statement] can be used inside the loop.

### 3. Check how many birds visited yesterday

- The second last element has a fixed index (remember to start counting from zero).

### 4. Check the count for a specific day

- The `Array` class has a [built-in method][array-indexof] to check at which index an element can be found. A special value is returned if a matching element could not be found.

### 5. Check what the counts were last week

- As this method does _not_ depends on the current week's count, it is defined as a [`static` method][static-members].
- There are [several ways to define an array][single-dimensional-arrays].

[array-foreach]: https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/arrays/using-foreach-with-arrays
[single-dimensional-arrays]: https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/arrays/single-dimensional-arrays
[fields]: https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/classes-and-structs/fields
[static-members]: https://www.oreilly.com/library/view/programming-c/0596001177/ch04s03.html
[array-indexof]: https://docs.microsoft.com/en-us/dotnet/api/system.array.indexof?view=netcore-3.1#System_Array_IndexOf_System_Array_System_Object_
[if-statement]: https://docs.microsoft.com/en-us/dotnet/csharp/language-reference/keywords/if-else
