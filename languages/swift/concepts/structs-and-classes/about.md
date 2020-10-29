[Structs and classes][structs-and-classes] are two of the primary building blocks of Swift programming. They are both means of grouping together related data and functions into self-contained units of functionality. All of the basic types that you have been working with up to this point, such as `Int`, `Double`, and `Character` as well as collection types like `Array`, `String`, and `Dictionary` are actually structs. And when you define a struct or class, you are defining a new type to be used within Swift, just as you used those types.

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

Here we have a struct that contains the statistics that will be associated with a character in a game and a struct that contains information about the character. Note that the `Character` class contains an instance of the `CharacterStats` struct inside of it.

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

For classes, where a property is not defined with a default value, an initializer is required.

Additional initializing functionality is available in Swift, though it is out of the scope of this exercise and will be covered in a separate exercise.

### Accessing properties

Struct and class properties can be accessed using _dot notation_ where the name of the value is followed by a dot (`.`) and the name of the property. If a property of a struct or class has properties of its own, this dot notation can be used to access these nested properties as well.

This notation can be used both to retrieve the property's value and, where allowed, to change it.

```swift
someStats.health
// => 0
differentStats.health
// => 100
someCharacter.name
// => nil

someStats.health = 87.3
someStats.health
// => 87.3
someCharacter.name = "Luther"
someCharacter.name
// => "Luther"
someCharacter.stats = differentStats
someCharacter.stats.health
// => 100
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
myChar.name = "Shadowfly"
myChar.characterClass = "spy"
myChar.active = true

myChar.active
// => true
myChar.sayName()
// => "My name is Shadowfly, my class is spy"
myChar.lift(750)
// => "Ooof! No way."
myChar.takesDamage(80)
myChar.active
// => false
```

### Identity

Because two different names may hold a reference to the same instance of class, it is sometimes useful to test if this is the case. Swift uses the `===` _identical_ operator to test for this (along with its negation, `!==`).

```swift
let char1 = GameCharacter()
let char2 = char1
let char3 = GameCharacter()

char1 === char2
// => true
char1 === char3
// => false
char2 === char3
// => false
char2 !== char3
// => true
```

Because no two names can refer to the same instance of a struct, these identity operators are not defined for structs.

```struct
someStats === differentStats
// Error: Cannot convert value of type 'CharacterStats' to expected argument type 'AnyObject?'
```

Note that identity is not the same as equality. By many definitions `char1` and `char3` would be considered equal as all of their properties hold the same values, but they do not share the same identity. Identity is made available automatically for all classes in Swift, but developers usually need to write their own equality operators to determine if two instances of a struct or class are _equal_. Doing this is beyond the scope of this exercise.

### Using structs vs. classes

While it may seem as though classes would be favored in general use over structs in Swift as they offer more functionality and aren't burdened with overhead of copying, the opposite is actually true.

Swift, as a language, opts for preferring safety as a core design principle. And classes are harder to reason about with regard to safety, given that their contents can be changed in unexpected ways thanks to being reference types. Because of this Swift prefers the usage of structs to classes and is optimized in many ways with the assumption that structs will be used in most cases. This is why we see that nearly every type in the standard library and in much of Apple's _Foundation_ library are structs.

But this doesn't mean that classes don't have their uses. In the words of Brent Royal-Gordon, a member of the Swift team at Apple:

> Use a class when the object represents a specific _thing_ that can't be duplicated without consequence; use a struct (or enum) when the instance represents some abstract data with no concrete existence. Some examples using framework types:
>
> - A piece of text is just some data; duplicating it doesn't do anything except maybe use more memory. So `String` should be a struct.
>
> - A label is a particular thing that exists at a particular place on screen; if you duplicated it, you'd end up with two labels on the screen, or with an off-screen copy of a label that wouldn't have any effect when you mutated it. So `UILabel` should be a class.
>
> - A URL is just some data; if you construct two URLs with the same contents, they are completely interchangeable. So `URL` should be a struct.
>
> - A connection to a web server to retrieve the contents of a URL is a particular thing; if you duplicated it, you would either establish another connection, or the two instances would interfere with each other (e.g. canceling one would cancel the other). So `URLSessionTask` and `NSURLConnection` are classes.
>
> Sometimes the same problem, approached in slightly different ways, would allow you to use either one. For instance, a database record is a particular _thing_ and should probably be a class, but copy the values of the fields (perhaps omitting the ID) out of it and suddenly you have a plausible struct. As a _general_ rule, it's usually better to use structs where possible because it's easier to reason about their behavior—mutations in one function don't suddenly pop up in a totally unrelated function—but sometimes a particular type works very easily as a class and very awkwardly as a struct. Ultimately, it's a judgement call.

[structs-and-classes]: https://docs.swift.org/swift-book/LanguageGuide/ClassesAndStructures.html
