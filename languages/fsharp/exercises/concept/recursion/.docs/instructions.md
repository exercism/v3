In this exercise you're working at a pizza place that delivers to customers.

You offer three types of pizzas:

- Margherita: \$7
- Caprese: \$9
- Formaggio: \$10

Customers can also choose two additional options for a small additional fee:

1. Extra sauce: \$1
1. Extra toppings: \$2

When customers place and order, an additional fee is added if they only order one or two pizzas:

- 1 pizza: \$3
- 2 pizzas: \$2

You have three tasks, each of which will work with pizzas and their price.

### 1. Define the pizza types and options

Define the `Pizza` discriminated union to represent the pizza types and the two additional options that can be added to a pizza:

- `Margherita`
- `Caprese`
- `Formaggio`
- `ExtraSauce`
- `ExtraToppings`

### 2. Calculate the prize of pizza

Implement the `pizzaPrice` function to calculate a pizza's price:

```fsharp
pizzaPrice Caprese
// => 9
```

### 3. Calculate the prize of an order

Implement the `orderPrice` function to calculate a pizza order's price:

```fsharp
orderPrice [Margherita; Formaggio]
// => 19
```
