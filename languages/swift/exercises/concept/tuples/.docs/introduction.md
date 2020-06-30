[Tuples][tuples] are used to combine multiple values into a single compound value where each of the values may have a different type. The compound value has a type that is the combination of the individual values' tyoes.

```swift
let order: (String, Double, Int) = ("biscuits", 2.99, 3)
```

The individual items of a tuple may be accessed by appending a `.n` to the tuple's name where _n_ is the index of the element you would like to access, starting with 0.

```swift
let orderItem = order.0
// => "biscuits"
let orderQuantity = order.2
// => 3
```

The components of a tuple may also be accessed through _decomposition_:

```swift
let (teamName, wins, losses, draws) = ("Loons", 15, 11, 8)
let points = 3 * wins + draws
// => 53
```

Unneeded values can be ignored in decomposition by using an `_` in place of a name:

```swift
let (_, wins, _, draws) = ("Loons", 15, 11, 8)
let points = 3 * wins + draws
// => 53
```

You can provide more semantic information about a tuple by supplying labels to the components of a tuple. The individual elements can then be accessed by either their index or their label:

```swift
let cindy: (name: String, age: Int) = (name: "Cindy", 28)
let cindyName = cindy.name
// => "Cindy"
let cindyAge = cindy.1
// => 28
```

If the tuple is defined as a variable, the individual components can be updated by assigning new values to their labels or indices.

```swift
var birdSightingLog = (week: 28, birds: ["robin", "blue jay", "heron"])
birdSightingLog.0 = 29
// => (week 29, ["robin", "blue jay", "heron"])
birdSightingLog.birds.append("swift")
// => (week 29, ["robin", "blue jay", "heron", "swift"])
```

[tuples]: https://docs.swift.org/swift-book/LanguageGuide/TheBasics.html#ID329
