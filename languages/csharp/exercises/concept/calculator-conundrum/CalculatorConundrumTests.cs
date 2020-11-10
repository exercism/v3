using Xunit;
using System;

public class SimpleCalculatorTests
{
    //Addition tests
    [Fact]
    public void Addition_with_small_operands()
    {
        Assert.Equal("22 + 25 = 47", CalculatorConundrum.Calculate(22, 25, "+"));
    }

    [Fact(Skip = "Remove this Skip property to run this test")]
    public void Addition_with_large_operands()
    {
        Assert.Equal("378961 + 399635 = 778596", CalculatorConundrum.Calculate(378_961, 399_635, "+"));
    }

    //Multiplication tests
    [Fact(Skip = "Remove this Skip property to run this test")]
    public void Multiplication_with_small_operands()
    {
        Assert.Equal("3 * 21 = 63", CalculatorConundrum.Calculate(3, 21, "*"));
    }

    [Fact(Skip = "Remove this Skip property to run this test")]
    public void Multiplication_with_large_operands()
    {
        Assert.Equal("72441 * 2048 = 148359168", CalculatorConundrum.Calculate(72_441, 2_048, "*"));
    }

    //Division tests
    [Fact(Skip = "Remove this Skip property to run this test")]
    public void Division_with_small_operands()
    {
        Assert.Equal("72 / 9 = 8", CalculatorConundrum.Calculate(72, 9, "/"));
    }

    [Fact(Skip = "Remove this Skip property to run this test")]
    public void Division_with_large_operands()
    {
        Assert.Equal("1338800 / 83675 = 16", CalculatorConundrum.Calculate(1_338_800, 83_675, "/"));
    }

    [Fact(Skip = "Remove this Skip property to run this test")]
    public void Calculate_throws_exception_for_division_with_0()
    {
        Assert.Equal("Division by zero is not allowed.", CalculatorConundrum.Calculate(33, 0, "/"));
    }

    // Invalid operator
    [Fact(Skip = "Remove this Skip property to run this test")]
    public void Calculate_throws_exception_for_non_valid_operations()
    {
        Assert.Throws<ArgumentOutOfRangeException>(() => CalculatorConundrum.Calculate(1, 2, "**"));
    }

    [Fact(Skip = "Remove this Skip property to run this test")]
    public void Calculate_throws_exception_for_null_as_operation()
    {
        Assert.Throws<ArgumentNullException>(() => CalculatorConundrum.Calculate(1, 2, null));
    }

    [Fact(Skip = "Remove this Skip property to run this test")]
    public void Calculate_throws_exception_for_empty_string_as_operation()
    {
        Assert.Throws<ArgumentException>(() => CalculatorConundrum.Calculate(1, 2, ""));
    }
}
