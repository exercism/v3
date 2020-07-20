You have a number of pizza slice shops in your town and you want to write a webapp that will let you compare two different pizza configurations to let you know who will give you the bigger slice.

## 1. Write a function to compute slice sizes which returns nil for invalid input.

Implement a function, `sliceSize(diameter: Double?, slices: Int?) -> Double?`, which, given the diameter of a pizza and the number of slices per pizza returns the area of a slice. For negative diameters and for number of slices less than 1, return nil, as there is no such thing as a pizza with negative diameter and no way to slice a pizza into fewer than 1 slice. If either parameter is `nil`, also return `nil`

```swift
sliceSize(diameter: 16, slices: 12)
// => 16.75516081914556
sliceSize(diameter: nil, slices: 8)
// => nil
```

## 2. Process input from your web application to determine the larger slice.

You web application will pass four strings to your function, `biggestSlice(diameterA: String, slicesA: String, diameterB: String, slicesB: String) -> String`. The first and second strings are the diameter and number of slices of the first pizza respectively, and the third and fourth are the diameter and number of slices of the second pizza respectively.

Implement `biggestSlice` so that it attempts to convert the diameter and number of slices for each pizza into a `Double` and an `Int` respectively if both of these values can be obtained from the strings, use your first function to try to compute the area, otherwise the area for that slice is `nil`. Once the areas of both slices are obtained, compare the two areas using the following rules:

1. If slice A's area is a `Double` and slice B's is `nil`, return "Slice A is bigger". If the reverse is true, return "Slice B is bigger".
2. If both areas are `Double`s, return "Slice A is bigger" or "Slice B is bigger" according to which slice has the greater area.
3. If both areas are `nil`, or if both are `Double`s and they are equal, return "Neither slice is bigger".

```swift
biggestSlice(diameterA: "10", slicesA: "6", diameterB: "14", slicesB: "12")
// => Slice A is bigger
biggestSlice(diameterA: "10", slicesA: "6", diameterB: "12", slicesB: "8")
// => Slice B is bigger
biggestSlice(diameterA: "Pepperoni", slicesA: "6", diameterB: "Sausage", slicesB: "12")
// => Neither slice is bigger
```
