While working at _Instruments of Texas_, you are tasked to work on an experimental calculator written in C#. Your team is having a problem with some operations raising errors and crashing the process. You have been tasked to write a function which wraps the operation function so that the errors can be handled in the normal program flow.

A static class, `TestHarnessOperations` has been provided for you and should not be
modified.

## 1. Complete the definition of the user-defined exception `CalculationException`

Complete the definition of the constructor of `CalculationException` which will need to store the exception
that it wraps as well as the operand that is being processed at the
time the exception is thrown.

## 2. Implement the `Calculate` routine

The `Calculate` routine should call the `TestHarnessOperations.HandleInt` routine
passing in the value that is passed into `Calculate`. `HandleInt` is
guaranteed to throw an `Sytem.OverflowException`. This exception
should be wrapped in a `CalculationException` and the value being
passed around should be stored as the operand. The newly
created exception should be thrown.

```csharp
var cth = new CalculatorTestHarness();
cth.Calulate(123);
// => throws an instance of CalculationException
```

## 3. Implement the Run routine

Implement a method `Run` tthat can either call
`Calculate` or `TestHarnessOperations.HandleInt` directly depending on the value
of the first (test name) argument to the method. It catches an instance of either
`CalculationException` or a `System.OverflowException` and the method
returns an appropriate message to the caller (see below). If
the operand contained in the instance of `CalculationException` is 0 then
a special message is returned.

If an unrecognized test name is passed in then an empty string is returned.

The example code below shows the appropriate values for the
parameters and corresponding messages to be returned.

```csharp
var cth = new CalculatorTestHarness();
cth.Run("Calculate", 0);
// => "Calculate failed for a zero value. " + innerException.Message

cth.Run("Calculate", 123);
// => "Calculate failed for a non-zero value. " + innerException.Message

cth.Run("HandleInt", 42);
// => "FakeOp failure"

cth.Run("Foo", 123);
// => string.Empty
```
