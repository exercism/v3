using System;
using Xunit;

public class UserDefinedExceptionsTests
{
    [Fact]
    public void Call_TestMultiplication()
    {
        var cth = new CalculatorTestHarness(new Calculator());

        Assert.Throws<CalculationException>(() => cth.TestMultiplication(Int32.MaxValue, Int32.MaxValue));
    }

    [Fact /*(Skip = "Remove this Skip property to run this test")*/]
    public void Call_Multiply_with_negative()
    {
        var cth = new CalculatorTestHarness(new Calculator());
        Assert.Equal("Multiply failed for a negative value. Arithmetic operation resulted in an overflow.",
            cth.Multiply(-2, Int32.MaxValue));
    }

    [Fact /*(Skip = "Remove this Skip property to run this test")*/]
    public void Call_Multiply_with_positive()
    {
        var cth = new CalculatorTestHarness(new Calculator());
        Assert.Equal("Multiply failed for a positive value. Arithmetic operation resulted in an overflow.",
            cth.Multiply(Int32.MaxValue, Int32.MaxValue));
    }

    [Fact /*(Skip = "Remove this Skip property to run this test")*/]
    public void Call_Multiply_with_success()
    {
        var cth = new CalculatorTestHarness(new Calculator());
        Assert.Equal("Multiply succeeded", cth.Multiply(6, 7));
    }

}
