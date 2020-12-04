Your friend Chandler is planning to visit exotic countries all around the world. But sadly, Chandler's math skill is not so good. He is so worried about get scammed at currency exchange during his trip. Chandler wants you to make a currency calculator for him. Here's his specifications about currency exchange calculator.

## 1. Estimating exchangeable value

Create the function `estimate_value()` where `budget` and `exchange_rate` are the two required parameters:

1. `budget` is amount of money you are planning to exchange.
2. `exchange_rate` is unit value of foreign currency.

This function should return the estimated value of foreign currency you can get based on your budget and current exchange rate.

**Note:** If your currency is USD and you want to exchange it into EUR, exchange rate is 1.20 if 1 EUR = 1.20 USD.

```python
>>> estimate_value(127.5, 1.2)
106.25
```

## 2. Changes after exchanging

Create the function `get_change()` where `budget` and `exchanging_value` are the two required parameters:

1. `budget` is amount of money you own.
2. `exchanging_value` is amount of money you want to exchange now.

This function should return the amount of changes after exchanging `exchanging_value`.

```python
>>> get_changes(127.5, 120)
7.5
```

## 3. Calculate value of bills

Create the function `get_value()` with parameters `denomination` and `number_of_bills`

1. `denomination` is value of single bill.
2. `number_of_bills` is amount of bills you got.

You need to return the total value of bills you got now.

```python
>>> get_value(5, 128)
640
```

## 4. Calculate number of bills

Create the function `get_number_of_bills()` with parameter `budget` and `denomination`

1. `budget` is amount of money you are planning to exchange.
2. `denomination` is value of single bill.

You need to return the number of bills after exchanging all your money.

```python
>>> get_number_of_bills(127.5, 5)
25
```

## 5. Calculate exchangeable value

Create the function `exchangeable_value()` with parameter `budget`, `exchange_rate`, `spread`, and `denomination`.

1. `budget` is amount of money you are planning to exchange.
2. `exchange_rate` is unit value of foreign currency.
3. `spread` is percentage value about exchanging fee.
4. `denomination` is value of single bill.

You need to return the maximum value you can get considering your budget, exchange rate, spread, and denomination.

**Note:** If 1 EUR is 1.20 USD and spread is 10%, the actual exchange rate of 1 EUR is 1.32 USD.

```python
>>> exchangeable_value(127.25, 1.20, 10, 20)
80
>>> exchangeable_value(127.25, 1.20, 10, 5)
95
```
