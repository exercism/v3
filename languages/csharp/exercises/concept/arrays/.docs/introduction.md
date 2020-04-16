In C#, data structures that can hold zero or more elements are known as _collections_. An **array** is a collection that has a fixed size and whose contains must all be of the same type. Elements can be assigned to an array or retrieved from it using an index, with the first element's index being zero:

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

Arrays can be manipulated by either calling an array's methods or properties, or by using the methods defined in the `Array` class.

Besides access values by index, the fact that an array is also a _collection_ means that you can iterate over all its values using a `foreach` loop:

```csharp
char[] vowels = new [] { 'a', 'e', 'i', 'o', 'u' };

foreach (char vowel in vowels)
{
    // Output the vowel
    System.Console.Write(vowel);
}

// => aeiou
```
