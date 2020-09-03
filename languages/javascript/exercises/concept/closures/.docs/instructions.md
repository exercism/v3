Your design company has primarily been working with CSS transformations to build webpages. After some discussion, a decision is made
to start using JavaScript to perform some of the calculations dynamically. Some of your teammates are less experienced with JavaScript,
so you decide to use a function closure to create reusable transformation for `{x, y}` coordinate pairs.

## 1. Translate the coordinates

Create a function which returns a function making use of a closure to perform a repeatable 2d translation of a coordinate pair.

```javascript
const moveCoordinatesRight2Px = translate2d(2, 0)
const result = moveCoordinatesRight2Px(4, 8)
// result => [6, 8]
```

## 2. Scale the coordinates

Create a function which returns a function making use of a closure to perform a repeatable 2d scale of a coordinate pair.

> For the purposes of this exercise, assume only positive scaling values.

```javascript
const doubleScale = scale2d(2, 2)
const result = doubleScale(6, -3)
// result => [12, -6]
```

## 3. Compose transformation functions

Combine two transformation functions to perform a repeatable transformation. This is often called _function composition_, where the result of the first function _'f(x)'_ is used as the input to the second function _'g(x)'_.

```javascript
const moveCoordinatesRight2Px = translate2d(2, 0)
const doubleCoordinates = scale2d(2, 2)
const composedTransformations = composeTransformation(
  moveCoordinatesRight2Px,
  doubleCoordinates
)
const result = composedTransformations(0, 1)
// result => [4, 2]
```

## 4. Save the results of functions

Make a function which _memoizes_ which takes a function, then returns a new function which remembers the inputs to the supplied function so that the value can be "remembered".

> Memoizing is sometimes called _dynamic programming_, it allows for expensive operations to be done only once, since their result is remembered.

```javascript
const tripleScale = scale2d(3, 3)
const memoizedScale = memoizeTransform(tripleScale)

memoizedScale(4, 3) // => [12, 9], this is computed since it hasn't been computed before for the arguments
memoizedScale(4, 3) // => [12, 9], this is remembered, since it was computed already
```
