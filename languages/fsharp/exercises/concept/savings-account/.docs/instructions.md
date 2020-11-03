In this exercise you'll be working with savings accounts. Each year, the balance of your savings account is updated based on its interest rate. The interest rate your bank gives you depends on the amount of money in your account (its balance):

- -3.213% for a negative balance.
- 0.5% for a positive balance less than `1000` dollars.
- 1.621% for a positive balance greater or equal than `1000` dollars and less than `5000` dollars.
- 2.475% for a positive balance greater or equal than `5000` dollars.

Each year the government allows you donate a percentage of your money to charity, tax free. Because you're a nice person, if your balance is positive at the end of the year, you donate twice this amount to charities, rounded down to the nearest whole dollar.

You have three tasks, each of which will deal your balance and its interest rate.

## 1. Calculate the interest rate

Implement the `interestRate` function to calculate the interest rate based on the specified balance:

```fsharp
interestRate 200.75m
// => 0.5f
```

Note that the value returned is a `single`.

## 2. Calculate the annual balance update

Implement the `annualBalanceUpdate` function to calculate the annual balance update, taking into account the interest rate:

```fsharp
annualBalanceUpdate 200.75m
// => 201.75375m
```

Note that the value returned is a `decimal`.

## 3. Calculate how much money to donate

Implement the `amountToDonate` function to calculate how much money to donate to charities based on the balance and the tax-free percentage that the government allows:

```fsharp
let balance = 550.5m
let taxFreePercentage = 2.5
amountToDonate balance taxFreePercentage
// => 27
```

Note that the value returned is an `int`.
