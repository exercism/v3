While working at _Instruments of Texas_, you are tasked to work on an experimental calculator written in C#. You are building a test harness to verify a number of calculator functions starting with multiplication. You will see that there is particular concern when the two operands of the multiplication are negative.

The `Calculator` class has been provided for you and should not be modified.

## 1. Complete the definition of the user-defined exception `CalculationException`

Complete the definition of the constructor of `CalculationException` which will need to store (wrap) any exception thrown by the calculator as well as the operands that are being processed at the time the exception is thrown.

## 2. Implement the `Multiply()` method

Implement the `CalculatorTestHarness.Multiply()` method, which should call the `Calculator.Multiply()` method of the `Calculator` instance passed to the constructor.
passing in x and y integer values. If an overflow occurs in the `Calculator.Multiply()` method, it will throw an `OverflowException`. This exception should be caught in the  `CalculatorTestHarness.Multiply()` method and wrapped in a `CalculationException` and the x and y values being passed around should be stored as the exception's operands. The newly created `CalculationException` object should be thrown. You can ignore the value returned by the `Calculator.Multiply()` method if it is successful.

```csharp
var cth = new CalculatorTestHarness(new Calculator());
cth.Multiply(Int32.MaxValue, Int32.MaxValue);
// => throws an instance of CalculationException

var cth2 = new CalculatorTestHarness(new Calculator());
cth2.Multiply(3, 2);
// => silently exits
```

## 3. Implement the `TestMultiplication()` method

Implement the `CalculatorTestHarness.TestMultiplication()` method which takes two integers and calls the `CalculatorTestHarness.Multiply()` method. There are three possible return values:

- `"Multiply succeeded"`: returned if the multiplication was successful (no exception was thrown).
- `"Multiply failed for negative operands. <INNER_EXCEPTION_MESSAGE>"`: returned if a `CalculationException` was thrown by the `CalculatorTestHarness.Multiply()` method and both integers are negative (less than zero).
- `"Multiply failed for mixed or positive operands. <INNER_EXCEPTION_MESSAGE>"`: returned if a `CalculationException` was thrown by the `CalculatorTestHarness.Multiply()` method and neither of the integers are negative (less than zero). 

 The `<INNER_EXCEPTION_MESSAGE>` placeholder hould be replaced with the `CalculationException`'s inner exception's message.

```csharp
var cth = new CalculatorTestHarness(new Calculator());
cth.TestMultiplication(Int32.MaxValue, Int32.MaxValue);
// => "Multiply failed for mixed or positive operands. " + innerException.Message

cth.TestMultiplication(-2, -Int32.MaxValue);
// => "Multiply failed for negative operands. " + innerException.Message

cth.Multiply(6, 7);
// => "Multiply succeeded"
```
