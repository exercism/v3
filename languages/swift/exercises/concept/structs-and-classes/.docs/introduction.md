Structs and classes are two of the primary building blocks of Swift programming. They are both means of grouping together related data and functions into self-contained units of functionality. And when you define a struct or class, you are defining a new type to be used within Swift, just as you used the types you've already worked with, like `Int` and `String`.

There are many similarities between structs and classes in Swift. Among other similarities, both are able to store values in _properties_ and provide functionality through the use of _methods_.

They each provide some additional functionality, which is out of scope for this exercise.

### Defining structs and classes

Both structs and classes are defined in roughly the same way. They start with the appropriate keyword followed by the type name that is being defined, then the body of the struct/class follows, placed between curly braces.

The body may consist of stored properties, which are defined and behave just like regular constants or variables.

```swift
struct CharacterStats {
  var health = 0.0
  var speed = 0
  var strength = 0
}

class GameCharacter {
  var stats = CharacterStats()
  var characterClass: String?
  var name: String?
  var active = false
  let id = makeRandomID()
}
```

### Instances

As noted above, defining a struct or class is just defining a new _type_. It is just the blueprint for what the values of that type will look like, but it does not actually create any values of that type for you to work with.

In order to create an _instance_ of that type, you need to write the name of the type followed by a pair of parentheses.

```swift
let someStats = CharacterStats()
let someCharacter = GameCharacter()
```

This will create values of these types, where the properties are populated with the default values supplied in the definition. Note that in optional cases like GameCharacter's `name` property, unless a value is provided, the property will default to nil, just like defining regular optional types where a value is not immediately provided.

With structs, Swift automatically provides something called a _memberwise initializer_, where values for the structs properties may be provided inside the parentheses which will override the default values in the definition.

```swift
let differentStats = CharacterStats(health: 100.0, speed: 6, strength: 18)
```

### Accessing properties

Struct and class properties can be accessed using _dot notation_ where the name of the value is followed by a dot (`.`) and the name of the property. If a property of a struct or class has properties of its own, this dot notation can be used to access these nested properties as well.

This notation can be used both to retrieve the property's value and, where allowed, to change it.

```swift
someStats.health
// => 0
someCharacter.name
// => nil

someStats.health = 87.3
someStats.health
// => 87.3
someCharacter.name = "Luther"
someCharacter.name
// => "Luther"

someCharacter.id  = "new id"
// Error: Cannot assign to property: 'id' is a 'let' constant
```

### Methods

Like properties, which store data in your structs and classes, you may also define _methods_ which store functions in your struct or class.

Methods are defined in the same way as a regular function, only inside the body of the struct or class. Note that if a function changes the value of a property in a struct, it must be preceded by the `mutating` keyword. Additionally, if a property can be changed by a method, that property must be defined using `var` rather than `let`, just like regular variables.

```swift
struct CharacterStats {
  var health = 0.0
  var speed = 0
  var strength = 0

  mutating func takeHit(_ damage: Double) {
    health = max(0.0, health - damage)
  }
  func canLift(_ weight: Int) -> Bool {
    weight < strength * 100
  }
}

class GameCharacter {
  var stats = CharacterStats()
  var characterClass: String?
  var name: String?
  var active = false
  let id: String = makeRandomID()

  func takesDamage(_ damage: Double) {
    stats.takeHit(damage)
    if stats.health <= 0 {
      active = false
    }
  }
  func sayName() -> String {
    return "My name is \(name ?? "no one"), my class is \(characterClass ?? "undetermined")"
  }
  func lift(_ weight: Int) -> String {
    if stats.canLift(weight) {
      return "No problem!"
    } else {
      return "Ooof! No way."
    }
  }
}
```

These methods can be called using dot notation, just like properties.

```swift
var myChar = GameCharacter()
myChar.stats = CharacterStats(health: 72.8, speed: 19, strength: 6)
myChar.active = true

myChar.lift(750)
// => "Ooof! No way."
myChar.takesDamage(80)
myChar.active
// => false
```

### Self

Instances of structs and classes each have an implicit value named `self` which refers to the instance itself. There are multiple uses for `self`, but it is most commonly used to disambiguate the names of properties and methods of the struct/class when there may be some confusion.

```swift
struct MySelf {
  var x = 0

  mutating func move(x: Int) {
    // here if we just say x = x it is unclear if we mean
    // the property x or the method parameter x, so we use
    // self for clarity
    self.x = x
  }
}
```
