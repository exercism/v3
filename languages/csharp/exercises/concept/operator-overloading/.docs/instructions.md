You've been tempted back to Hyperia (with the high inflation) for an eye watering daily rate.

The Central Bank is contemplating introducing the US Dollar as a second currency so all the accounting systems have to be adapted to handle multiple currencies.

You have been asked to implement the currency object.

## 1. Enable the currency to be tested for equality

Please modify the `Currency` struct to handle equality. If the two currency structs do not have the same unit ("USD" or "HD") then an instance of `ArgumentException` should be thrown.

```csharp
Currency amountA = new Currency(55, "HD");
Currency amountB = new Currency(55, "HD");
Currency amountC = new Currency(55, "USD");
amountA == amountB
// => true
amountA != amountB
// => false
amountA == amountC
// => ArgumentException
amountA != amountC
// => ArgumentException
```

## 2. Compare currency amounts

Please modify the `Currency` struct to handle comparisons (`>` and `<`, only). If the two currency structs do not have the same unit ("USD" or "HD") then an instance of `ArgumentException` should be thrown.

```csharp
Currency amountA = new Currency(55, "HD");
amountA > new Currency(50, "HD")
// => true
amountA < new Currency(50, "HD")
// => false
amountA > new Currency(50, "USD")
// => ArgumentException
```

## 3. Add and subtract currency amounts

Please modify the `Currency` struct to handle arithmetic addition and subtraction (`+` and `-`, only). If the two currency structs do not have the same unit ("USD" or "HD") then an instance of `ArgumentException` should be thrown.

```csharp
Currency amountA = new Currency(55, "HD");
Currency amountB = new Currency(100, "HD");
Currency amountC = new Currency(55, "USD");
amountA + amountB
// => {155, HD}
amountB - amountA
// => {45, HD}
amountA + amountC
// => ArgumentException
```

## 4. Multiply and divide currency amounts

Please modify the `Currency` struct to handle arithmetic multiplication and division (`*` and `/`, only).

```csharp
Currency amountA = new Currency(10, "HD");
amountA * 2m
// => {20, HD}
amountB / 2m
// => {5, HD}
```

## 5. Convert the currency amount to a double

Please modify the `Currency` struct so that an instance can be explicitly cast to a double.

```csharp
Currency amountA = new Currency(55.5m, "HD");
(double)amountA
// => 55.5
```
