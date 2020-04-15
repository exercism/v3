Data structures that can hold zero or more elements are known as _collections_. An array is a collection with a fixed size and which elements must all be of the same type. Elements in an array can be assigned to and retrieved by index, with the first element's index being zero:

```csharp
// Declare array with explicit size (size is 2)
int[] twoIntegers = new int[2];

// Assign first and second element by index
twoIntegers[0] = 7;
twoIntegers[1] = 8;

// Retrieve the second element by index
twoIntegers[1] == 8; // => true
```

Arrays can also be defined using a shortcut notation that allows you to both create the array and set its value. As the compiler can now tell how many elements the array will have, the length can be omitted:

```csharp
// Assign and initialize (size is 3)
double[] threeDoublesV1 = new double[] { 1.2, 4.45, 9.998 };

// Shorter notation (size is 3)
double[] threeDoublesV2 = new[] { 1.2, 4.45, 9.998 };

// Shortest notation (size is 3)
double[] threeDoublesV3 = { 1.2, 4.45, 9.998 };
```

Arrays can be manipulated by either calling an array's [methods][array-methods] or [properties][array-properties], or by using the methods defined in the [`Array` class][array-class].

An array is also a _collection_, which means that you can iterate over all its values using a `foreach` loop:

```csharp
char[] vowels = new [] { 'a', 'e', 'i', 'o', 'u' };

foreach (char vowel in vowels)
{
    // Output the vowel
    System.Console.Write(vowel);
}

// => aeiou
```

One could use a `for` loop to iterate over an array:

```csharp
char[] vowels = new [] { 'a', 'e', 'i', 'o', 'u' };

for (int i = 0; i < vowels.Length; i++)
{
    // Output the vowel
    System.Console.Write(vowels[i]);
}

// => aeiou
```

However, generally a `foreach` loop is preferrable over a `for` loop for the following reasons:

- A `foreach` loop is guaranteed to iterate over _all_ values. With a `for` loop, it is easy to miss elements, for example due to an off-by-one error.
- A `foreach` loop is more _declarative_, your code is communicating _what_ you want it to do, instead of a `for` loop that communicates _how_ you want to do it.
- A `foreach` loop works on all collection types, includding those that don't support using an indexer to access elements.

To guarantee that a `foreach` loop will iterate over _all_ values, the compiler will not allow updating of a collection within a `foreach` loop:

```csharp
char[] vowels = new [] { 'a', 'e', 'i', 'o', 'u' };

foreach (char vowel in vowels)
{
    // This would result in a compiler error
    // vowel = 'Y';
}
```

[implicitly-typed-arrays]: https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/arrays/implicitly-typed-arrays
[array-foreach]: https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/arrays/using-foreach-with-arrays
[single-dimensional-arrays]: https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/arrays/single-dimensional-arrays
[array-class]: https://docs.microsoft.com/en-us/dotnet/api/system.array?view=netcore-3.1
[array-properties]: https://docs.microsoft.com/en-us/dotnet/api/system.array?view=netcore-3.1#properties
[array-methods]: https://docs.microsoft.com/en-us/dotnet/api/system.array?view=netcore-3.1#methods
