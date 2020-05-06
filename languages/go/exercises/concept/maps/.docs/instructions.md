A friend of you has an old wholesale store called **Gross Store**, the name came from the quantity of the item that the store sell, it's all in [gross unit][gross-unit]. Your friend asked you to implement a point of sale (POS) system for his store, **but first, you want to build a prototype for it, in your prototype, your system will only record the quantity**. Your friend gave you a list of measurements to help you:

| Unit               | Score |
| ------------------ | ----- |
| quarter_of_a_dozen | 3     |
| half_of_a_dozen    | 6     |
| dozen              | 12    |
| small_gross        | 120   |
| gross              | 144   |
| great_gross        | 1728  |

## 1. Store the unit of measurement in your program

In order to use the measurement, you need to store the measurement in your program.

## 2. Add item to the customer bill

To implement this, you'll need to:

- Check whether the given unit of measurement is correct
- Add the item to the customer bill, indexed by the item name. You probably also need a variable to represent the customer bill, you are expected to use `map`

```go
ok := AddItem("carrot", "dozen")
fmt.Println(ok)
// Output: true or false
```

> Note that the returned value is type of `bool`

## 3. Remove item from the customer bill

To implement this, you'll need to:

- Check whether the given item is in the bill
- Check whether the given unit of measurement is correct
- Check whether the new quantity is less than 0, is so return `false`
- Check whether the new quantity is 0, is so return remove the item from the customer bill
- Otherwise reduce the quantity of the item

```go
ok := RemoveItem("carrot", "dozen")
fmt.Println(ok)
// Output: true or false
```

> Note that the returned value is type of `bool`

## 4. Return the number of specific item that is in the customer bill

To implement this, you'll need to:

- Check whether the given item is in the bill
- Otherwise, return the quantity of the item

```go
qty, ok := GetItem("carrot")
fmt.Println(qty)
// Output: 12
fmt.Println(ok)
// Output: true or false
```

> Note that the returned value are types of `int` and `bool`

## 5. Simulate checkout operation

This function supposed to do a payment to the customer bank, since you're developing a prototype, what you need to do is:

- Reset the customer bill

```go
Checkout()
```

> Note that there's no value returned by this function

[gross-unit]: https://en.wikipedia.org/wiki/Gross_(unit)
