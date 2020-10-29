## Value types v. reference types

While Swift has many different types, each of these types fall into one of two different classifications, _value types_ and _reference types_. In Swift, [structs are value types][structs-are-value-types] while [classes are reference types][classes-are-reference-types].

The primary difference between value types and reference types is that, when value types are assigned to a variable or constant, or when passed into a function, it is actually a copy of the value that is assigned or passed, not that particular value. Modifying one copy of a struct will not affect other copies. With reference types, a reference to that value is assigned or passed, so two different names can refer to the same instance of a class. This can lead to modification of the original instance when accessed under a different name.

Consider the following code which uses a class to implement a character in a game and a struct to implement a character's statistics:

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

We can now create some instances of these two items and see how value types and reference types differ in behavior.

```swift
var statsA = CharacterStats(health: 100.0, speed: 15, strength: 12)
var statsB = statsA
statsA.speed = 17
statsB.takeHit(50)
statsA.health
// => 100
statsB.health
// => 50
statsA.speed
// => 17
statsB.speed
// 15

var charA = GameCharacter()
charA.name = "Hagor the Brave"
charA.characterClass = "Warrior"
charA.stats = statsA
charA.sayName()
// => "My name is Hagor the Brave, my class is Warrior"
charA.lift(1000)
// => true

var charB = charA
charB.name = "Bunny Wigglesworth"
charB.characterClass = "Couch Potato"
charB.stats.strength = 3

charA.sayName()
// => "My name is Bunny Wigglesworth, my class is Couch Potato"
charA.lift(1000)
// => false
```

Note how changing `charB`'s properties affected `charA`'s, as both names refer to the same instance. Note also how, with the two character's stats, that value types within a reference type will change within the reference types. This is not a violation of value type behavior, however, because `charA.stats` is the same instance of `CharacterStats` as `charB.stats`, since `charA` and `charB` refer to the same instance of the `GameCharacter` class. As we can see, copies of those value types will not affect each other, for example.

```swift
var aStats = charA.stats
aStats.speed = 1
charA.takesDamage(30)

aStats.speed
// => 1
charA.stats.speed
// => 17
aStats.health
// => 100
charA.stats.health
// => 70
```

This is why structs require the `mutating` keyword in front of methods that alter the state of the struct's properties. Changing the property of a struct actually creates a new copy of the struct with the new value set for that property and the new copy replaces the old one. It is a visual indicator that this copying and replacing may take place for values of that type.

This has the side-effect that any instances of structs where variable properties change, either via calling mutating functions or by direct assignment _must_ be assigned to variables, not constants. Otherwise a compiler error will be generated:

```swift
let constantStats = CharacterStats(health: 30, speed: 20, strength: 10)
constantStats.health
// => 30
constantStats.speed
// => 20
constantStats.takeHit(10)
// => Cannot use mutating member on immutable value: 'constantStats' is a 'let' constant
constantStats.speed -= 2
// => Left side of mutating operator isn't mutable: 'constantStats' is a 'let' constant
constantStats.strength = 15
// => Cannot assign to property: 'constantStats' is a 'let' constant
```

The initial values can still be created and accessed, but they are not allowed to change. This restriction does not apply to classes. If an instance of a class is assigned to a name using `let`, it is the reference to the instance that the constant holds and thus can not change, not the instance itself. So the instance itself can change, but the reference cannot be changed to point to a different instance.

A summary of what may be changed with structs and classes defined with `let` or `var` is as follows

|         | **struct**                                                                                  | **class**                                                                                  |
| ------- | ------------------------------------------------------------------------------------------- | ------------------------------------------------------------------------------------------ |
| **let** | _Cannot_ assign a new struct to the name.</br>_Cannot_ modify any properties of the struct. | _Cannot_ assign a new class to the name.</br>_Can_ modify mutable properties of the class. |
| **var** | _Can_ assign a new struct to the name.</br>_Can_ modify mutable properties of the struct.   | _Can_ assign a new class to the name.</br>_Can_ modify mutable properties of the class.    |

[structs-are-value-types]: https://docs.swift.org/swift-book/LanguageGuide/ClassesAndStructures.html#ID88
[classes-are-reference-types]: https://docs.swift.org/swift-book/LanguageGuide/ClassesAndStructures.html#ID89
