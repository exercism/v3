using Xunit;
using System;
public class SimpleCalculatorTests
{
    //Addition tests
    [Fact]
    public void Addition_with_small_operands()
    {
        Assert.Equal(47, SimpleCalculator.Calculate(22, 25, "+"));
    }

    [Fact(Skip = "Remove this Skip property to run this test")]
    public void Addition_with_large_operands()
    {
        Assert.Equal(778_596, SimpleCalculator.Calculate(378_961, 399_635, "+"));
    }

    [Fact(Skip = "Remove this Skip property to run this test")]
    public void Addition_that_overflows()
    {
        Assert.Equal(-1, SimpleCalculator.Calculate(Int32.MaxValue, 5, "+"));
        Assert.Equal("Result invalid: Result of operation does not fit in type of int.", SimpleCalculator.ErrorLog);
    }

    //Multiplication tests
    [Fact(Skip = "Remove this Skip property to run this test")]
    public void Multiplication_with_small_operands()
    {
        Assert.Equal(63, SimpleCalculator.Calculate(3, 21, "*"));
    }

    [Fact(Skip = "Remove this Skip property to run this test")]
    public void Multiplication_with_large_operands()
    {
        Assert.Equal(148_359_168, SimpleCalculator.Calculate(72_441, 2_048, "*"));
    }

    [Fact(Skip = "Remove this Skip property to run this test")]
    public void Multiplication_that_overflows()
    {
        Assert.Equal(-1, SimpleCalculator.Calculate(50_000, 50_000, "*"));
        Assert.Equal("Result invalid: Result of operation does not fit in type of int.", SimpleCalculator.ErrorLog);
    }

    //Division tests
    [Fact(Skip = "Remove this Skip property to run this test")]
    public void Division_with_small_operands()
    {
        Assert.Equal(8, SimpleCalculator.Calculate(72, 9, "/"));
    }

    [Fact(Skip = "Remove this Skip property to run this test")]
    public void Division_with_large_operands()
    {
        Assert.Equal(16, SimpleCalculator.Calculate(1_338_800, 83_675, "/"));
    }

    [Fact(Skip = "Remove this Skip property to run this test")]
    public void Calculate_throws_exception_for_division_with_0()
    {
        Assert.Throws<DivideByZeroException>(() => SimpleCalculator.Calculate(33, 0, "/"));
    }

    // Invalid operator
    [Fact(Skip = "Remove this Skip property to run this test")]
    public void Calculate_throws_exception_for_non_valid_operations()
    {
        Assert.Throws<InvalidOperationException>(() => SimpleCalculator.Calculate(1, 2, "**"));
    }
}