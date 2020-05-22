A dictionary is a collection of elements where each element comprises a key and value such that if a key is passed to a method of the ditionary its associated value is returned. It has the same role as maps or associative arrays do in other languages.

A dictionary can be created as follows:

```csharp
new Dictionary<int, string>{{1, "One"}, {2, "Two"}};
// 1 => "One", 2 => "Two"
```

Note that the key and value types are part of the definition of the dictionary.

Entries can be added to the dictionary as follows:

```csharp
var numbers = new Dictionary<int, string>();
numbers.Add(3, "Three");
numbers.Add(4, "Four");
// 3 => "Three", 4 => "Four"
```

To remove a key-value pair simply pass the key to the `Remove` method.

A dictionary lookup takes the following form:

```csharp
var numbers = new Dictionary<int, string>{{1, "One"}, {2, "Two"}};
numbers[2];
// "Two"
```

This is known as an [indexer][indexer-properties].
]

A value in the dictionary can be replaced as follows:

```csharp
var numbers = new Dictionary<int, string>{{1, "One"}, {2, "Two"}};
numbers[2] = "Deux";
// {{1, "One"}, {2, "Deux"}}
```

You can test if a value exists in the dictionary with:

```csharp
var dict = new Dictionary<string, string>{/*...*/};
dict.ContainsKey("some key that exists");
// => true
```

You can enumerate the following from the dictionary, keys, values, key-value pairs:

```csharp
var dict = new Dictionary<string, int>{/*...*/};

Dictionary<string, int>.KeyCollection keys = dict.Keys;

Dictionary<string, int>.ValueCollection values = dict.Values;

IEnumerable<KeyValuePair<string,int>> enumerable = dict;
```

[indexer-properties]: https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/indexers/
