using Xunit;

public class NullabilityTests
{
    [Fact]
    public void FullName() =>
        Assert.Equal("Matilda Leana Cox", Badge.ComputeNameText("Matilda", "Leana", "Cox"));

    [Fact]
    public void MissingMiddleName() =>
        Assert.Equal("Matilda Cox", Badge.ComputeNameText("Matilda", null, "Cox"));

    [Fact]
    public void OnlyFirstName() =>
        Assert.Equal("Matilda", Badge.ComputeNameText("Matilda", null, null));

    [Fact]
    public void OnlyLastName() =>
        Assert.Equal("Cox", Badge.ComputeNameText(null, null, "Cox"));

    [Fact]
    public void FontSize() =>
        Assert.Equal(3*17, Badge.ComputeWidthPx("Cox", 17));

    [Fact]
    public void NothingToDisplay() =>
        Assert.Equal(0, Badge.ComputeWidthPx(null, 17));

    [Fact]
    public void NoFontSize() =>
        Assert.Equal(0, Badge.ComputeWidthPx("Matilda", null));

}
