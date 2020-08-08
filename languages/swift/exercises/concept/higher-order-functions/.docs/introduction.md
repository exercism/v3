### Function types

We have seen before how function signatures are defined using a function name followed by comma-separated list of parameter labels along with their types, followed by `->` and the type of the values returned by the function.

If one were to remove all of the names and labels from this signature, they would be left with the type of the function.

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

func printShoppingList(item: String, quantity: Int) {
  print("You need to buy \(quantity) \(item).")
}
// printShoppingList: (String, Int) -> ()
```

Notice how the types of the first two functions are the same, `(String, Int) -> String`, even though the two functions work veery differently. They both take a `String` and an `Int` as input and return a `String`. And, as one can see from the third function's type, even though the function doesn't appear to return anything, it actually returns `()`, the only value of type `Void`. Note that `()` and `Void` are interchangeable as type names, so the type of `printShoppingList` could also be written as `String, Int) -> Void`.

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
// Error: Cannot assign value of type '(String, Int) -> ()' to type '(String, Int) -> String'
```

### Function types as parameter types

As function types can be used anywhere other types can be used, it follows that they can be used as parameter types and passed into other functions and called from within those functions.

```swift
func distribute(function f: (String, Int) -> String, strings: [String], times i: Int) -> [String] {
  var result = [String]()
  for string in strings {
    result.append(f(string, i))
  }
  return result
}
// distribute: ((String, Int) -> String, [String], Int) -> [String]

distribute(function: shoppingList, strings: ["carrots", "eggs", "popsicles"], times: 3)
// => ["You need to buy 3 carrots.", "You need to buy 3 eggs.", "You need to buy 3 popsicles."]

distribute(function: repeater, strings: ["carrots", "eggs", "popsicles"], times: 3)
// => ["carrotscarrotscarrots", "eggseggseggs", "popsiclespopsiclespopsicles"]

func apply(functions fs: [(String, Int) -> String], string: String, i: Int) -> [String] {
  var result = [String]()
  for f in fs {
    result.append(f(string, i))
  }
  return result

}
// apply: ([(String, Int) -> String], String, Int) -> [String]


apply(functions: [shoppingList, repeater], string: "carrots", i: 3)
// => ["You need to buy 3 carrots.", "carrotscarrotscarrots"]
```

### Function types as return types

Similarly, function types may be used as return types for functions. In other words, one can write functions that create and return other functions. When creating these functions, the function that is returned can use the other parameters passed into the function or the local variables created inside the parent function.

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

### Higher-order functions

Functions that take functions as parameters and/or return functions are known as higher order functions. Swift has many higher-order functions in its various libraries. 
