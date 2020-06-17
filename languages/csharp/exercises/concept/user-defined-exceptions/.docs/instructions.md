While working at _Instruments of Texas_, you are tasked to work on an experimental calculator written in C#. You are building a test harness to verify a number of calculator functions starting with multiplication. You will see that there is particular concern when the two operands of the multiplication are negative.

The `Calculator` class has been provided for you and should not be
modified.

## 1. Complete the definition of the user-defined exception `CalculationException`

Complete the definition of the constructor of `CalculationException` which will need to store (wrap) any exception thrown by the calculator as well as the operands that are being processed at the time the exception is thrown.

## 2. Implement the `Multiply()` method

The `Multiply()` routine should call the `Calclator.Multiply()` routine
passing in x and y integer values. If an overflow occurs then an `OverflowException` will be thrown by the `Calculator`. This exception should be caught in `Multiply` and wrapped in a `CalculationException` and the x and y values being passed around should be stored as the exception's operands. The newly created `CalculationException` object should be thrown. The test has no interest in the value returned by `Calculator.Multiply` if it is successful.

```csharp
var cth = new CalculatorTestHarness(new Calculator());
cth.Multiply(Int32.MaxValue, Int32.MaxValue);
// => throws an instance of CalculationException

var cth2 = new CalculatorTestHarness(new Calculator());
cth2.Multiply(3, 2);
// => silently exits
```

## 3. Implement the `TestMultiplication()` method

- `TestMultiplication()` takes two integers and calls `Multiply`.
- If all goes well it returns "Multiply succeeded".
- If an exception is thrown by `Nultiply` this is caught.
- The error message returned depends on whether or not both the operands are negative. See the examples below for exact text.

```csharp
var cth = new CalculatorTestHarness(new Calculator());
cth.TestMultiplication(Int32.MaxValue, Int32.MaxValue);
// => "Multiply failed for mixed or positive operands. " + innerException.Message

cth.TestMultiplication(-2, -Int32.MaxValue);
// => "Multiply failed for negative operands. " + innerException.Message

cth.Multiply(6, 7);
// => "Multiply succeeded"
```
