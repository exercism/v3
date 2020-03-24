using System;
using Xunit;

public class NullabilityTests
{
    [Fact]
    public void FalseWhenStringNotNull() =>
        Assert.False(Nullability.IsNull("hello"));

    [Fact]
    public void TrueWhenStringNull() =>
        Assert.True(Nullability.IsNull(null));

    [Fact]
    public void SumLengthsWhenStringNotNull() =>
        Assert.Equal(10, Nullability.TotalLength("hello", "world"));

    [Fact]
    public void ExceptionWhenFirstStringNull() =>
        Assert.Throws<NullReferenceException>(() => Nullability.TotalLength("hello", null!));

    [Fact]
    public void ExceptionWhenSecondStringNull() =>
        Assert.Throws<NullReferenceException>(() => Nullability.TotalLength(null!, "world"));

    [Fact]
    public void SmartSumLengthsWhenStringNotNull() =>
        Assert.Equal(10, Nullability.TotalLength("hello", "world"));

    [Fact]
    public void SmartSumLengthsWhenFirstStringNull() =>
        Assert.Equal(5, Nullability.TotalLengthSmart("hello", null));

    [Fact]
    public void SmartSumLengthsWhenSecondStringNull() =>
        Assert.Equal(5, Nullability.TotalLengthSmart(null, "world"));
}
