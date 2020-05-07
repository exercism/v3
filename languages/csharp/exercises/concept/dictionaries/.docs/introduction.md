A dictionary is a collection of elements where each element comprises a key and value such that if a key is passed to a method of the ditionary its associated value is returned.  It has the same role as maps or associative arrays do in other languages.

A dictionary can be created as follows:

```csharp
var numbers = new Dictionary<int, string>{{1, "One"}, {2, "Two"}};
``` 

Note that the key and value types are part of the definition of the dictionary.

Entries can be added to the dictionary as follows:

```csharp
var numbers = new Dictionary<int, string>();
numbers.Add(3, "Three");
numbers.Add(4, "Four");
``` 

To remove a key-value pair simply pass the key to the `Remove` method.

A dictionary lookup takes the following form:

```csharp
var numbers = new Dictionary<int, string>{{1, "One"}, {2, "Two"}};
var xx = number[2];
// "Two"
``` 

A value in the dictionary can be replaced as follows:

```csharp
var numbers = new Dictionary<int, string>{{1, "One"}, {2, "Two"}};
number[2] = "Deux";
// {{1, "One"}, {2, "Deux"}}
``` 

