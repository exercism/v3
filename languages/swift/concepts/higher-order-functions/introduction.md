### Function types

If one were to remove all of the names and labels from a Swift function signature, they would be left with the type of the function.

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

Notice that the types of the first two functions are the same, `(String, Int) -> String`, even though the two signatures are different and the two functions work very differently. They both take a `String` and an `Int` as input and return a `String`. And, as one can see from the third function's type, even though the function body doesn't appear to return anything, it actually implicitly returns `()`, the only value of type `Void`.

These function types can be used in the same manner as any other type in Swift, for example, they can be used in the declaration of variables or constants. And since functions are values in Swift, already existing functions can be assigned to these constants/variables:

```swift
var stringAndIntToString: (String, Int) -> String

stringAndIntToString = shoppingList
stringAndIntToString("carrots", 3)
// => "You need to buy 3 carrots."
```

As with other variables/constants, the value you assign to it must be of the correct type:

```swift
stringAndIntToString = printShoppingList
// Error: Cannot assign value of type '(String) -> ()' to type '(String, Int) -> String'
```

### Function types as parameter types

As function types can be used anywhere other types can be used, it follows that they can be used as parameter types and passed into other functions and called from within those functions.

```swift
func apply3(to str: String, function f: (String, Int) -> String) -> String {
  f(str,3)
}
// apply3: ([String], (String, Int) -> String) -> [String]

apply3(to: "eggs", function: shoppingList)
// => "You need to buy 3 eggs."

apply3(to: "eggs", function: repeater)
// => "eggseggseggs"
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
