In this exercise you're going to write a calculator for rectangles.

Given is a case class `Rectangle`. A rectangle is defined by its width and its height:

```scala
case class Rectangle(width: Int, height: Int)
```

The calculator will allow to sum two rectangles and to expand a rectangle by multiplying it with a number.

## 1. Allow rectangles of the same height to be added

Implement a `+` method that takes another rectangle as argument. Assume that the passed rectangle has the same height as the current one. The method should return a new rectangle, whose width is a sum of both rectangle's widths and whose height is the same as of both rectangles.

Let's visualize the expected outcome:

![sum of 2 rectangles of same height](sum_same_height.png)

This code snippet shows how to use the `+` method:

```scala
val leftRectangle = Rectangle(2, 5)
val rightRectangle = Rectangle(1, 5)
val sumRectangle = leftRectangle + rightRectangle
println(sumRectangle)
// => Rectangle(3, 5)
```

## 2. Allow adding rectangles of different heights

How to deal with rectangles of different heights? In this task please update the `+` method like follows: The new rectangle should get the height of the higher rectangle.

Let's visualize again the expected outcome:

![sum of 2 rectangles of same height](sum_different-height.png)

This is a code example shows again the expected result:

```scala
val leftRectangle = Rectangle(2, 4)
val rightRectangle = Rectangle(1, 1)
val sumRectangle = leftRectangle + rightRectangle
println(sumRectangle)
// => Rectangle(3, 4)
```

## 3. Allow multiplying rectangles by a factor

Implement a `*` method, which takes an `Int` parameter and returns an expanded rectangle, whose width and height are multiplied by the given parameter.

![multiplication of a rectangle with a number](multiplication_rectangle_number.png)

This code snippet shows the usage of the new `*` method.

```scala
val originalRectangle = Rectangle(2, 3)
val expandedRectangle = originalRectangle * 3
println(expandedRectangle)
// => Rectangle(6, 9)
```
