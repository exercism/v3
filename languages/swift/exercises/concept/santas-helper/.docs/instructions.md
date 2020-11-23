You are an elf working in Santa Claus' logistics division and you have been given a pair of tasks from the boss for upgrading the system's software.

## 1. Convert coordinates to polar

Your first task is updating the complex number array that helps run Santa's sleigh. The boss discovered that the last development team stored the arrays using Cartesian coordinates rather than polar coordinates, which everyone knows are the boss' favorite.

For this task you will need to implement the function `cartesianToPolar(_:)` that takes in a tuple, `(x: Double, y: Double)`, and return a new tuple, `(r: Double, phi: Double)`, where:

- r = √(x<sup>2</sup> + y<sup>2</sup>)
- phi = _atan2_(y, x)

See this image for an example

![cartesian to polar coordinates](https://en.wikipedia.org/wiki/Polar_coordinate_system#/media/File:Polar_to_cartesian.svg)

```swift
let coordinate = (x: -78.70524308742053, y: -39.243573777212724)
cartesianToPolar(coordinate)
// => (r: 87.94642330565522, phi: -2.6790540755626306)
```

## 2. Merge two database records

For your second task, your boss is upgrading its database and is modifying its record structures. They want to combine the records from the toy production database and the toy recipient databases to save space and give the product leads better visibility into the demand for their toy.

For this task you will implement the function `combineRecords(production:gifts:)` that takes two tuples as input, `production: (toy: String, id: Int, productLead: String)` and `gifts: (Int, [String])` and your function will return a combined tuple, `(id: Int, toy: String, productLead: String, recipients: [String])`. Note that the two `id` fields in the input tuples are guaranteed to be the same value.

```swift
combineRecords(
  production: (
    toy: "Chemistry set",
    id: 328509,
    productLead: "Binkles"
 ),
  gifts: (
    328509,
    ["Inés", "Maxime", "Bandile", "Shaurya", "Екатерина"]
  )
)
// => (id: 328509, toy: "Chemistry set", productLead: "Binkles", recipients: ["Inés", "Maxime", "Bandile", "Shaurya", "Екатерина"])
```
