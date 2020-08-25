The built-in Swift types can each represent a finite number of values. These numbers are usually quite high, e.g. 2<sup>32</sup> different `Float`s 2<sup>64</sup> different `Int`s, ~2<sup>20</sup> different `Character`s, and a number of strings bounded only by the amount of storage on your computer. On the flip side, there are a handful of types that offer just a few values. The `Never` type has no values, `Void` has just `()`, and `Bool` has just `true` and `False`.

This leads to problems when someone wants to model something that has some other number of possible values, like the 63 official HTTP status codes, or the 8 buttons of the NES controller. One may be forced to use existing types like strings or ints to represent their values, similar to what was done with the HTTP status codes, but this leads to a few different issues. One issue is that, while a computer may be fine processing valid values like 103, 226, and 505, the program itself also has to constantly check that it doesn't receive invalid values like 213, 427, 512, or 601. At the same time the _programmer_ is responsible for ensuring that every valid value is checked when examining a code to act upon.

Another issue is that a human reading (or writing) a program can have troubles recalling that the values 103, 226, and 505 really represent "Early Hints", "IM Used", and "HTTP Version Not Supported". One could mitigate this by using a string to represent the text version of the code, but this can cause errors when one tries to compare an incoming status code to "HTTP Version not Supoorted". These errors are common and hard to track down.

As an approach to solving this problem, Swift, like many modern languages offers a language feature it calls [_Enumerations_][enumerations] or _Enums_ for short.

Enums in Swift are a mechanism of creating new types which are inhabited by a finite number of named values which may carry additional associated information, and which can be checked at compile time for completeness and accuracy of use. Additionally, enums can have properties and methods attached to them, providing additional information and functionality based on the current value of the enum.

### Defining Enums

The most basic enums are defined in Swift by the `enum` keyword followed by the name of the type and then the body of the enum enclosed in curly braces, which includes a list of the values of the enum, introduced with the `case` keyword. Following the [Swift style guide][api-design-guidelines], the name of the type should be in UpperCamelCase while the values should be in lowerCamelCase.

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

This defines a new type named `NESButtons` with possible values `up`, `down`, `left`, `right`, `a`, `b`, `select`, and `start`. These values can be referred to by following the name of the type followed by a dot (`.`) and the value. In cases where the type name can be inferred, only the dot and value are needed. These values can then be used like any other values in Swift.

```swift
var lastPressed = NESButton.up

// Now that the type of lastPressed is set as NESButton
lastPressed = .down

let konamiCode = [NESButton.up, .up, .down, .down, .left, .right, .left, .right, .b, .a]
```

Notice that, since all of the elements of an array must be of the same type, we only need to supply the type name for one of the elements, the rest can be just the values. The type name wouldn't be required for any of them if the constant was given the proper type annotation, e.g. `let konamiCode: [NESButton] = …`.

### Enums and switch statements

Enums are frequently used [alongside `switch` statements][enums-and-switches] which are used to determine the action to take based on the value of the enum.

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

Note that one can fix the above error by using a `default` case in the `switch`.

```swift
lastPressed = .select
switch lastPressed {
case .up, .down, .left, .right:
  print("You pressed a direction button.")
default:
  print("You pressed a non-direction button.")
}
// prints: "You pressed a non-directional button."
```

However, this automatically makes the `switch` exhaustive, so if we were to add, say, diagonal direction cases, like `.upAndLeft` to the enum, we would not know to update that switch and we would wrongly be told that a non-directional button was pressed. To prevent this, it is better to explicitly list all cases where possible.

```swift
lastPressed = .select
switch lastPressed {
case .up, .down, .left, .right:
  print("You pressed a direction button.")
case .a, .b, .select, .start:
  print("You pressed a non-direction button.")
}
// prints: "You pressed a non-directional button."
```

Since the compiler knows all possible values of an enum, it can also detect errors like misspellings that creep into programs where strings are used to represent values.

```swift
lastPressed = .selcet
// Error: Type 'NESButton' has no member 'selcet'
```

### Methods

Like other types in Swift, enums may contain methods which allow the enum to provide functionality based on the current value of the enum.

Methods are analogous to functions, only they are defined inside the body of the enum and they are tied to the current enum value. They are accessed via _dot notation_ where the name of the enum value is followed by a dot (`.`) and the name of the method and its parameters.

Inside the method, the enum value can be referred to as `self`, and in the type signature, if one is accepting as a parameter or returning a value of the the enum they can refer to the type as `Self`.

#### Initializers

Initializers are special methods that are used to set up a value of the enum. Their definition looks a lot like that of a method only there is no `func` keyword, no return type, and the name must be `init` and the initializer _must_ assign a value of the enum to self. Initializers are called either via dot notation or by passing the initializer's parameters to the name of the enum.

Enums may also have failable initializers which return optional enum values. These are used when there may be no valid value to initialize to based on the input to the initializer and `nil` may be assigned to self instead. In these cases, the initializer name is written as `init?`, though the question mark is left off when the initializer is called.

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

Enums can also carry with them an internal value known as a [_raw value_][raw-values]. The raw values must all be of the same type, which is declared in the definition of the enum. So we could assign, e.g. Character values to our `NESButton` enum by altering the definition:

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
// => Optional(.up)
let invalid = NESButton.init(rawValue: "🙄")
// => nil
```

Swift can implicitly assign raw values for `String` and `Int` raw values. If a raw value type of `String` is specified, the raw value will be implicitly assigned to the name on the value as a String unless the implicit value is overridden with an explicit assignment.

If a raw value type of `Int` is specified, the raw value will be implicitly assigned to an int 1 greater than the previous case's raw value, unless overridden with an explicit assignment. The default raw value for the first case is 0 unless otherwise specified.

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

Enums in Swift can also carry another type of information known as [_associated values_][associated-values]. With associated values, each case of the enum may have a different type of value that can be carried along with the enum value. The types of the associated values are specified in the enum declaration.

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

### Recursive enumerations

As seen in the previous example, the associated type of an enum's case can be of any type, including that of an enum. In some cases, it may be desired to make the type of an associated value be the type of the enum that contains that case. This can be done in Swift by writing the keyword `indirect` in front of each case that uses these [recursive associated types][recursive-enumerations], or by putting `indirect` in front of the `enum` keyword in front of the `enum` keyword to make all cases of the enum indirect.

```swift
enum BinaryTree {
  case leaf(value: Int)
  indirect case node(left: BinaryTree, value: Int, right: BinaryTree)
}

enum BinaryTree {
  case leaf(value: Int)
  indirect case node(left: BinaryTree, value: Int, right: BinaryTree)
}

let tree: BinaryTree = .node(left: .leaf(value: 1), value: 3, right: .node(left: .leaf(value: 5), value: 6, right: .leaf(value: 9)))
```

Note that enums can have either raw values, or associated values, or neither, but they cannot have both.

[api-design-guidelines]: https://swift.org/documentation/api-design-guidelines/
[enumerations]: https://docs.swift.org/swift-book/LanguageGuide/Enumerations.html
[enums-and-switches]: https://docs.swift.org/swift-book/LanguageGuide/Enumerations.html#ID147
[recursive-enumerations]: https://docs.swift.org/swift-book/LanguageGuide/Enumerations.html#ID536
[raw-values]: https://docs.swift.org/swift-book/LanguageGuide/Enumerations.html#ID149
[associated-values]: https://docs.swift.org/swift-book/LanguageGuide/Enumerations.html#ID148
