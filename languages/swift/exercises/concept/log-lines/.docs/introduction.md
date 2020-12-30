## enumerations

Enums in Swift are a mechanism of creating new types which are inhabited by a finite number of named values which may carry additional associated information, and can have properties and methods attached to them.

### Defining Enums

The most basic enums are defined as seen below.

```swift
enum NESButton {
  case up
  case down
  case left
  case right
  case a
  case b
  case select
  case start
}
```

For brevity, multiple cases can also be written on a single line, separated by commas, with a single `case` keyword.

```swift
enum NESButton {
  case up, down, left, right, a, b, select, start
}
```

This defines a new type named `NESButtons` with possible values `up`, `down`, `left`, `right`, `a`, `b`, `select`, and `start`. These values can be referred to by following the name of the type with a dot (`.`) and the value. In cases where the type name can be inferred, only the dot and value are needed.

### Methods

Like other types in Swift, enums may contain methods which allow the enum to provide functionality based on the current value of the enum.

Methods are analogous to functions, only they are defined inside the body of the enum and they are tied to the current enum value. They are accessed via _dot notation_ where the name of the enum value is followed by a dot (`.`) and the name of the method and its parameters.

Inside the method, the enum value can be referred to as `self`, and in the type signature, if one is accepting as a parameter or returning a value of the the enum they can refer to the type as `Self`.

#### Initializers

Initializers are special methods that are used to set up a value of the enum. Their definition looks a lot like that of a method only there is no `func` keyword, no return type, and the name must be `init` and the initializer _must_ assign a value of the enum to `self`. Initializers are called either via dot notation or by passing the initializer's parameters to the name of the enum.

```swift
enum Coin {
  case heads
  case tails

  init(_ i: Int) {
    if i.isMultiple(of: 2) {
      self = .heads
    } else {
      self = .tails
    }
  }

  func flip() -> Self {
    switch self {
    case .heads: return .tails
    case .tails: return .heads
    }
  }
}

let tails = Coin.init(13)
// .tails
let heads = Coin(0)
// .heads
let anotherTails = heads.flip()
// .tails
```

### Raw values

Enums can also carry with them an internal value known as a _raw value_. The raw values must all be of the same type, which is declared in the definition of the enum. So we could assign, e.g. Character values to our `NESButton` enum by altering the definition:

```swift
enum NESButton: Character {
  case up = "⬆️"
  case down = "⬇️"
  case left = "⬅️"
  case right = "➡️"
  case a = "🅰️"
  case b = "🅱️"
  case select = "✅"
  case start = "🚦"
}
```

Here, the `: Character` tells the compiler that the raw values will be of type `Character` and the assignment of the raw value follows each value.

Raw values can be accessed through each value's automatically generated `rawValue` property.

```swift
NESButton.left.rawValue
// => "⬅️"
NESButton.b.rawValue
// => "🅱️"
```

Swift can implicitly assign raw values for `String` and `Int` raw values. If a raw value type of `String` is specified, the raw value will be implicitly assigned to the name on the value as a String unless the implicit value is overridden with an explicit assignment.

If a raw value type of `Int` is specified, the raw value will be implicitly assigned to an int 1 greater than the previous case's raw value, unless overridden with an explicit assignment. The default raw value for the first case is 0 unless otherwise specified.

```swift
enum Coin: String {
  case heads
  case tails = "eagle"
}

Coin.heads.rawValue
// => "heads"
Coin.tails.rawValue
// => "eagle"

enum Dwarf: Int {
  case grumpy, sleepy, sneezy, happy = 8, bashful, dopey, doc
}

Dwarf.grumpy.rawValue
// => 0
Dwarf.sneezy.rawValue
// => 2
Dwarf.happy.rawValue
// => 8
Dwarf.bashful.rawValue
// => 9
```
