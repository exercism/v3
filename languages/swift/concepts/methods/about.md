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
