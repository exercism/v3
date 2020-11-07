As [dictionaries][dictionaries] are one of Swift's three primary collection types, knowing how to work with dictionaries is a big part of knowing how to program in Swift.

## Key Takeaways from this Exercise

- Dictionaries store mappings between _keys_ which are elements of one type and _values_
- Values can be of any type.
- Keys can be of any type that conforms to the [Hashable protocol][hashable-protocol-docs]. All of the basic Swift types have this property, and it can be added to other types.
- Dictionaries are unordered collections.
- Dictionaries can be defined using dictionary literals:

```swift
var addresses: Dictionary<String, String> = ["The Munsters": "1313 Mockingbird Lane", "The Simpsons": "742 Evergreen Terrace", "Buffy Summers": "1630 Revello Drive"]
var sequences: [String: [Int]] = ["Euler's totient": [1, 1, 2, 2, 4, 2, 6, 4], "Lazy caterer": [1, 2, 4, 7, 11, 16, 22, 29, 37], "Carmichael": [561, 1105, 1729, 2465, 2821, 6601, 8911, 10585, 15841]]
let constants: = ["pi": 3.14159, "e": 2.71828, "phi": 1.618033, "avogadro": 6.02214076e22]
```

- Empty dictionaries can be written by following the type name of the dictionary by a pair of parenthesis, e.g. `[Int: String]()`, or, if the type can be determined from the context, as just a pair of square brackets surrounding a colon, `[:]`.
- Type names for dictionaries are written in one of two ways: `Dictionary<K, V>` or `[K: V]` where `K` is the type of the keys in the dictionary and `V` is the type of the values.
- Dictionary elements can be accessed using subscript notation, e.g. `addresses["The Simpsons"]`.
- Values returned from these look-ups are optionals; `nil` is returned if a requested key is not present in the dictionary.```
- To avoid the optional type of the return, one can supply a default value to return if the key in not found in the dictionary.

```swift
let e: Double = constants["e", default: 0]
// => 2.71828
let hoagie: [Int] = sequences["Hoagie", default: []]
// => []
let betty = addresses["Betty Cooper", default: "Address unknown"]
// => "Address unknown"
```

- The subscript notation can be used to set the value associated with the supplied key, provided the dictionary was defined as a variable (i.e. using `var`).
- This can be used to update the value associated with a key or to add a new _key: value_ pair to the dictionary.
- Dictionaries can be sorted by passing a sorting function into the dictionary's `sorted(by:)` method.
- For-in loops can be used to iterate through the _key: value_ pairs of a dictionary
- Each pair is presented as a tuple and can be destructured like other tuples to assign each element to a name. For example, to print out the address book, on can write:

[The other methods][dictionary-docs] available for working with dictionaries can be seen in the Apple Swift API documentation.

[dictionaries]: https://docs.swift.org/swift-book/LanguageGuide/CollectionTypes.html#ID113
[dictionary-docs]: https://developer.apple.com/documentation/swift/Dictionary
[hashable-protocol-docs]: https://developer.apple.com/documentation/swift/hashable
