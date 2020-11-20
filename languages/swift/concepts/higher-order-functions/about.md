### Function types

We have seen before how function signatures are defined using a function name followed by comma-separated list of parameter labels along with their types, followed by `->` and the type of the values returned by the function.

If one were to remove all of the names and labels from this signature, they would be left with the [type of the function][function-types].

```swift
func shoppingList(item: String, quantity: Int) -> String {
  "You need to buy \(quantity) \(item)."
}
// shoppingList: (String, Int) -> String

func repeater(_ phrase: String, times: Int) -> String {
  var result = ""
  for _ in 1...times {
    result += phrase
  }
  return result
}
// repeater: (String, Int) -> String

func printString(line: String) {
  print(line)
}
// printString: (String) -> ()
```

Notice how the types of the first two functions are the same, `(String, Int) -> String`, even though the two functions work veery differently. They both take a `String` and an `Int` as input and return a `String`. And, as one can see from the third function's type, even though the function doesn't appear to return anything, it actually returns `()`, the only value of type `Void`. Note that `()` and `Void` are interchangeable as type names, so the type of `printString` could also be written as `String) -> Void`.

These types can be used in the same manner as any other type in Swift, for example, they can be used in the declaration of variables or constants. And since functions are values in Swift, already existing functions can be assigned to these constants/variables:

```swift
var stringAndIntToString: (String, Int) -> String

stringAndIntToString = shoppingList
stringAndIntToString("carrots", 3)
// => "You need to buy 3 carrots."

stringAndIntToString = repeater
stringAndIntToString("carrots", 3)
// => "carrotscarrotscarrots"
```

As with other variables/constants, the value you assign to it must be of the correct type:

```swift
stringAndIntToString = printShoppingList
// Error: Cannot assign value of type '(String) -> ()' to type '(String, Int) -> String'
```

### Function types as parameter types

As function types can be used anywhere other types can be used, it follows that [they can be used as parameter types][functions-as-parameters] and passed into other functions and called from within those functions.

```swift
func apply3(to str: String, function f: (String, Int) -> String) -> String {
  f(str,3)
}
// apply3: ([String], (String, Int) -> String) -> [String]

apply3(to: "eggs", function: shoppingList)
// => "You need to buy 3 eggs."

apply3(to: "eggs", function: repeater)
// => "eggseggseggs"

func apply(functions fs: [(String, Int) -> String],
           to string: String,
          ) -> [String] {
  var result = [String]()
  for f in fs {
    result.append(f(string, 3))
  }
  return result

}
// apply: ([(String, Int) -> String], String) -> [String]


apply(functions: [shoppingList, repeater], to: "carrots")
// => ["You need to buy 3 carrots.", "carrotscarrotscarrots"]
```

### Function types as return types

Similarly, [function types may be used as return types][functions-as-returns] for functions. In other words, one can write functions that create and return other functions. When creating these functions, the function that is returned can use the other parameters passed into the function or the local variables created inside the parent function.

```swift
func makeAdder(base: Int) -> (Int) -> Int {
  func adder(_ i: Int) -> Int {
    base + i
  }
  return adder
}
// makeAdder: (Int) -> (Int) -> Int

let add10 = makeAdder(base: 10)
// add10: (Int) -> Int

let subtract20 = makeAdder(base: -20)
// subtract10: (Int) -> Int

add10(5)
// => 15

subtract20(5)
// => -15
```

### Capturing values

As seen with the `makeAdder(base:)` function above, nested functions are able to access the parameters and variables of their surrounding functions. Additionally, they are able to maintain access to these values after the enclosing function terminates. This action of obtaining and maintaining access is known as [_capturing_][capturing-values].

### Higher-order functions

Functions that take functions as parameters and/or return functions are known as higher order functions. Swift has many higher-order functions in its various libraries. For example, the `sorted(by:)` method available on Swift collections such as `Array` which for an array with elements of type `T` takes a function of type `(T, T) -> Bool` that determines which of two values should come before the other in a sorted Array, returning `true` if the left-hand value should appear first and `false` if the right-hand value should appear first.

Another example is the `contains(where:)` method on collections which for a collection with elements of type `T`, takes a function of type `(T) -> Bool` which returns `true` if the element satisfies some property and `false` otherwise. `contains(where:)` then returns `true` if some element in the collection satisfies this property.

```swift
func longer(_ strA: String, _ strB: String) -> Bool {
  strA.count > strB.count
}
// longer: (String, String) -> Bool

["apple", "ball", "carrot"].sorted(by: longer)
// => ["carrot", "apple", "ball"]

["apple", "ball", "carrot"].sorted(by: >)
// => ["carrot", "ball", "apple"]


func isBig(_ i: Int) -> Bool {
  i > 100
}
// isBig: (Int) -> Bool

func isVowel(_ char: Character) -> Bool {
  switch char.lowercased() {
  case "a", "e", "i", "o", "u": return true
  default: return false
  }
}
// isVowel: (Character) -> Bool

[1, 75, 3, 99].contains(where: isBig)
// => false

"Owl".contains(where: isVowel)
// => true

```

[function-types]: https://docs.swift.org/swift-book/LanguageGuide/Functions.html#ID174
[functions-as-parameters]: https://docs.swift.org/swift-book/LanguageGuide/Functions.html#ID176
[functions-as-returns]: https://docs.swift.org/swift-book/LanguageGuide/Functions.html#ID177
[capturing-values]: https://docs.swift.org/swift-book/LanguageGuide/Closures.html#ID103
