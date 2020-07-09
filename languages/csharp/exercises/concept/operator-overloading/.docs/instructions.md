You've been tempted back to Hyperia (with the high inflation) for an eye watering daily rate.

The Central Bank is contemplating introducing the US Dollar as a second currency so all the accounting systems have to be adapted to handle two currencies.

You have been asked to implement the currency object.

## 1. Enable the currency to be tested for equality

Please modify the `Currency` struct to handle equality.  If the two currency structs do not have the same unit ("USD" or "HD") then an instance of `ArgumentException` should be thrown.

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

## 2. Comparison

## 3. Arithmetic + and -

## 4. Arithmentic * and /

## 5. The currency must be capable of converting to a double.

```csharp
Currency amountA = new Currency(55.5, "HD");
(double)amountA
// => 55.5
```
