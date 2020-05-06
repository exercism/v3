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

## 2. Implement `AddItem`

This function will add item to the customer bill, to implement this, you'll need to:

- Check whether the given unit of measurement is correct
- Add the item to the customer bill

```go
ok := AddItem("carrot", "dozen")
fmt.Println(ok)
// Output: true or false
```

## 3. Implement `RemoveItem`

This function will remove item from the customer bill, to implement this, you'll need to:

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

## 4. Implement `GetItem`

This function will return the number of specific item that the customer has in his/her bill, to implement this, you'll need to:

- Check whether the given item is in the bill
- Otherwise, return the quantity of the item

```go
qty, ok := GetItem("carrot")
fmt.Println(qty)
// Output: 12
fmt.Println(ok)
// Output: true or false
```

## 5. Implement `Checkout`

This function supposed to do a payment to the customer bank, since you're developing a prototype, you'll just need to:

- Reset the customer bill

```go
Checkout()
// Output:
```

[gross-unit]: https://en.wikipedia.org/wiki/Gross_(unit)
