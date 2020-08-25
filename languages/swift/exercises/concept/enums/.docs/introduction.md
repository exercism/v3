Enums in Swift are a mechanism of creating new types which are inhabited by a finite number of named values which may carry additional associated information, and which can be checked at compile time for completeness and accuracy of use. Additionally, enums can have properties and methods attached to them, providing additional information and functionality based on the current value of the enum.

### Defining Enums

The most basic enums are defined in Swift by the `enum` keyword followed by the name of the type and then the body of the enum enclosed in curly braces, which includes a list of the values of the enum, introduced with the `case` keyword. Following the Swift style guide, the name of the type should be in UpperCamelCase while the values should be in lowerCamelCase.

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

This defines a new type named `NESButtons` with possible values `up`, `down`, `left`, `right`, `a`, `b`, `select`, and `start`. These values can be referred to by following the name of the type followed by a dot (`.`) and the value. In cases where the type name can be inferred, only the dot and value are needed.

```swift
var lastPressed = NESButton.up

// Now that the type of lastPressed is set as NESButton
lastPressed = .down
```

These values can then be used like any other values in Swift.

```swift
let konamiCode = [NESButton.up, .up, .down, .down, .left, .right, .left, .right, .b, .a]
```

Notice how, since all of the elements of an array must be of the same type, we only need to supply the type name for one of the elements, the rest can be just the values. The type name wouldn't be required for any of them if the constant was given the proper type annotation, e.g. `let konamiCode: [NESButton] = …`.

### Enums and switch statements

Enums are frequently used alongside `switch` statements which are used to determine the action to take based on the value of the enum.

```swift
lastPressed = .left
switch lastPressed {
case .up: print("You pressed the UP button")
case .down: print("You pressed the DOWN button")
case .left: print("You pressed the LEFT button")
case .right: print("You pressed the RIGHT button")
case .a: print("You pressed the A button")
case .b: print("You pressed the B button")
case .select: print("You pressed the SELECT button")
case .start: print("You pressed the START button")
}

// prints "You pressed the LEFT button"
```

As `switch` statements are required to be exhaustive, the compiler will alert you if not all cases are represented.

```swift
switch lastPressed {
case .up, .down, .left, .right: print("You pressed a direction button.")
}
// Error: Switch must be exhaustive
```

This is especially helpful if cases are added to enums as a program evolves. The compiler can point out all of the locations where non-exhaustive switches need to be updated to include the new case.

Since the compiler knows all possible values of an enum, it can also detect errors like misspellings that creep into programs where strings are used to represent values.

```swift
lastPressed = .selcet
// Error: Type 'NESButton' has no member 'selcet'
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

Raw values can also be used to initialize values of the enum type. Note that the return value is an optional, as an invalid raw value may be passed in.

```swift
let upButton = NESButton.init(rawValue: "⬆️")
// => .up
let invalid = NESButton.init(rawValue: "🙄")
// => nil
```

Swift can implicitly assign raw values for `String` and `Int` raw values. If a raw value type of `String` is specified, the raw value will be implicitly assigned to the name on the value as a String unless the implicit value is overridden with an explicit assignment.

If a raw value type of `Int` is specified, the raw value will be implicitly assigned to an int 1 greater than the previous case's raw value, unless overridden with an explicit assignment. The default raw value for thee first case is 0 unless otherwise specified.

```swift
enum Coin: String {
  case heads
  case tails = "eagle"
}

Coin.head.rawValue
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

### Associated values

Enums in Swift can also carry another type of information known as _associated values_. With associated values, each case of the enum may have a different type of value that can be carried along with the enum value. The types of the associated values are specified in the enum declaration.

```swift
enum Suit: Character {
  case diamonds = "♦️", clubs = "♣️", hearts = "♥️", spades = "♠️"
}

enum Card {
  case ace(Suit), king(Suit), queen(Suit), jack(Suit)
  case number(Suit, Int)
}

let aceOfSpades = Card.ace(.spades)
let twoOfHearts = Card.number(.hearts, 2)
```

These values can then be extracted and used via pattern matching.

```swift
func printCard(_ card: Card) {
  switch card {
  case .ace(let suit): print("A\(suit.rawValue)")
  case .king(let suit): print("K\(suit.rawValue)")
  case .queen(let suit): print("Q\(suit.rawValue)")
  case .jack(let suit): print("J\(suit.rawValue)")
  case let .number(suit, value): print("\(value)\(suit.rawValue)")
  }
}

printCard(aceOfSpades)
// prints "A♠️"
printCard(twoOfHearts)
// prints "2♥️"
```
