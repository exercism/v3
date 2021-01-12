Dictionaries are one of Swift's three primary collection types. Dictionaries store mappings between _keys_ which are elements of one type and _values_ which are elements of another type (possibly the same type as that of the keys).

Dictionary literals are written as a series of `key: value` pairs, separated by commas, enclosed in square brackets. Empty dictionaries can be written by following the type name of the dictionary by a pair of parenthesis, e.g. `[Int: String]()`, or, if the type can be determined from the context, as just a pair of square brackets surrounding a colon, `[:]`. Type names for dictionaries are written in one of two ways: `Dictionary<K, V>` or `[K: V]` where `K` is the type of the keys in the dictionary and `V` is the type of the values. When creating an empty array, the type must be specified.

```swift
var addresses: Dictionary<String, String> = ["The Munsters": "1313 Mockingbird Lane", "The Simpsons": "742 Evergreen Terrace", "Buffy Summers": "1630 Revello Drive"]
var sequences: [String: [Int]] = ["Euler's totient": [1, 1, 2, 2, 4, 2, 6, 4], "Lazy caterer": [1, 2, 4, 7, 11, 16, 22, 29, 37], "Carmichael": [561, 1105, 1729, 2465, 2821, 6601, 8911, 10585, 15841]]
let constants = ["pi": 3.14159, "e": 2.71828, "phi": 1.618033, "avogadro": 6.02214076e22]
var emptyDict1: [Int: Int] = [:]
var emptyDict2 = ["Character": "String"]()
var emptyDict3 = Dictionary<Int, Double>()
```

Elements of a dictionary can be accessed using subscript notation, where the name of the dictionary is followed by square brackets which enclose the key for which we want the corresponding value.

Note, however, that the value returned is not of type _Value_, but rather of optional type, _Value?_. This is because one can supply a key that is not present in the dictionary. In these cases, Swift chooses to return `nil` rather than throw an error.

```swift
let munster: String? = addresses["The Munsters"]
// => "1313 Mockingbird Lane"
let carmichael = sequences["Carmichael"]
// => [561, 1105, 1729, 2465, 2821, 6601, 8911, 10585, 15841]
let planck = constants["planck"]
// => nil
```

To avoid the optional type of the return, one can supply a default value to return if the key if not found in the dictionary.

```swift
let e: Double = constants["e", default: 0]
// => 2.71828
let hoagie: [Int] = sequences["Hoagie", default: []]
// => []
let betty = addresses["Betty Cooper", default: "Address unknown"]
// => "Address unknown"
```

This subscript notation can be used to retrieve the value associated with that key as well as to set the value associated with that key, provided the dictionary was defined as a variable (i.e. using `var`). This can be used to update the value associated with a key or to add a new _key: value_ pair to the dictionary.

```swift
sequence["Euler's totient"]?.append(contentsOf: [6,4])
sequence["Euler's totient"]
// => [1, 1, 2, 2, 4, 2, 6, 4, 6, 4]
addresses["Betty Cooper"] = "111 Queens Ave."
addresses["Betty Cooper", default: "Address unknown"]
// => "111 Queens Ave."
constants["Gelfond's"] = 23.140692
// compiler error: "Cannot assign through subscript: 'constants' is a 'let' constant"
```

Like arrays, the _key: value_ pairs of a dictionary can be stepped through one at a time using a for-in loop. This type of loop takes each _key: value_ pair of the dictionary and binds the pair to a specified name for further processing inside the loop body. The pair is represented as a tuple and can be decomposed like other tuples to assign each element to a name. For example, to print out the address book, on can write:

```swift
for (name, address) in addresses {
  print("\(name) lives at \(address)")
}

// prints out:
// Betty Cooper lives at 111 Queens Ave.
// The Munsters lives at 1313 Mockingbird Lane
// The Simpsons lives at 742 Evergreen Terrace
// Buffy Summers lives at 1630 Revello Drive
```

Like the other collection types, dictionaries can be sorted according to arbitrary sorting functions. To do so, one must first define a function that takes two _key: value_ pairs, represented as tuples and returns a `Bool` such that the return value is `true` if the first _key: value_ pair should appear in the sorted result _before_ the second _key: value_ pair.

This function can then be passed into the dictionary's `sorted(by:)` method which will return a sorted array of _key: value_ pairs. E.g. to sort the `constants` dictionary by the value of the constant plus the number of characters in the constant's name we could write:

```swift
func weirdSort(_ lhs: (String, Double), _ rhs: (String, Double)) -> Bool {
  let left = lhs.1 + Double(lhs.0.count)
  let right = rhs.1 + Double(rhs.0.count)
  return left < right
}

let sortedConstants = constants.sorted(by: weirdSort)
// => [(key "e", value 2.71828), (key "phi", value 1.618033), (key "pi", value 3.14159), (key "avogadro", value 6.02214076e+22)]
```
