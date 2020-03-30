using Xunit;

public class NullabilityTests
{
    [Fact]
    public void ComputeWidthPx() =>
        Assert.Equal(7*12+5*12+3*12+2*12, Badge.ComputeWidthPx("Matilda", "Leana", "Cox", 12));

    [Fact]
    public void MissingMiddleName() =>
        Assert.Equal(7*12+3*12+12, Badge.ComputeWidthPx("Matilda", null, "Cox", 12));

    [Fact]
    public void OnlyFirstName() =>
        Assert.Equal(7*12, Badge.ComputeWidthPx("Matilda", null, null, 12));

    [Fact]
    public void OnlyLastName() =>
        Assert.Equal(3*12, Badge.ComputeWidthPx(null, null, "Cox", 12));

    [Fact]
    public void DifferentFontSize() =>
        Assert.Equal(3*17, Badge.ComputeWidthPx(null, null, "Cox", 17));

    [Fact]
    public void NothingToDisplay() =>
        Assert.Equal(0, Badge.ComputeWidthPx(null, null, null, 17));

    [Fact]
    public void NoFontSize() =>
        Assert.Equal(0, Badge.ComputeWidthPx("Matilda", null, null, null));

}
