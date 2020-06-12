While working at _Instruments of Texas_, you are tasked to work on an experimental calculator written in C#. You are building a test harness to verify a number of calculator functions starting with multiplication.

The `Calculator` class has been provided for you and should not be
modified.

## 1. Complete the definition of the user-defined exception `CalculationException`

Complete the definition of the constructor of `CalculationException` which will need to store (wrap) any exception thrown by the calculator as well as the operands that are being processed at the time the exception is thrown.

## 2. Implement the `TestMultiplication()` method

The `TestMultiplication()` routine should call the `Calclator.Multiplication()` routine
passing in x and y integer values. The y value is always positive, the x value may be negative. If an overflow occurs then an `OverflowException` will be thrown by the `Calculator`. This exception should be caught in `TestMultiplication` and wrapped in a `CalculationException` and the x and y values being passed around should be stored as the exception's operands. The newly created `CalculationException` object should be thrown. Otherwise the product of x and y should be returned by `TestMultiplication`.

```csharp
var cth = new CalculatorTestHarness(new Calculator());
cth.TestMultiplication(Int32.MaxValue, Int32.MaxValue);
// => throws an instance of CalculationException

var cth2 = new CalculatorTestHarness(new Calculator());
cth2.TestMultiplication(3, 2);
// => 6
```

## 3. Implement the `Multiply()` method

- `Multiply()` takes two integers and calls `TestMultiplication`.
- If all goes well it returns "Multiply succeeded".
- If an exception is thrown by `TestMultipliction` this is caught.
- An error message is returned depending on whether the first of the two operands was negative. See the examples below for exact text.

```csharp
var cth = new CalculatorTestHarness(new Calculator());
cth.Multiply(Int32.MaxValue, Int32.MaxValue);
// => "Multiply failed for a positive value. " + innerException.Message

cth.Multiply(-2, Int32.MaxValue);
// => "Multiply failed for a negative value. " + innerException.Message

cth.Multiply(6, 7);
// => "Multiply succeeded. " + innerException.Message
```
