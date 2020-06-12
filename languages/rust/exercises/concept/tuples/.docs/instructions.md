# Instructions

In this exercise you'll explore tuples in Rust by building a kitchen calculator. Imagine you are following a sensible gingersnap cookie recipe but it uses American units for measuring ingredients. You are most familiar with metric units. Being an intrepid programmer, you want to automate that work by writing a kitchen calculator.

Use the following chart in your program:

| Unit to convert | volume | in millilitres (mL) |
| --------------- | ------ | ------------------- |
| mL              | 1      | 1                   |
| US cup          | 1      | 240                 |
| US fluid ounce  | 1      | 30                  |
| US teaspoon     | 1      | 5                   |
| US tablespoon   | 1      | 15                  |

## 1. Extract the volume from a unit/volume pair

Implement the `get_volume` function that takes a unit/volume pair tuple as its parameter, and returns the volume part.

```rust
get_volume((Unit::USCup, 1))
// 1
```

## 2. Convert the tuple to millilitres

Implement the `to_millilitres` function that takes a unit/volume pair tuple as its parameter, and returns a new tuple with its volume converted to millilitres using the conversion chart.

```rust
to_millilitres((Unit::USCup, 2.5))
// (Unit::Millilitre, 600.0)
```

- Do the same for each `Unit` variant.

```rust
to_millilitres((Unit::Teaspoon, 1))
// (Unit::Millilitre, 5)
```

## 3. Convert the volume tuple in millilitres to another unit

- Given a tuple, `(Unit::Millilitre, 600.0)`, and the desired unit, `Unit::UsCup`, convert the volume to the desired unit using the conversion chart.

```rust
to_cup((Unit::Millilitre, 600.0))
// (Unit::UsCup, 2.5)
```

## 4. Convert between any units

- Given a volume tuple, `(Unit::UsCup, 2.5)` and destination unit, preform the conversion

```rust
convert((Unit::UsCup, 1), Unit::UsTablespoon)
// (Unit::UsTablespoon, 13.51488)
```
