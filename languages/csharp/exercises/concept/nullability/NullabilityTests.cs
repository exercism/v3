using Xunit;

public class NullabilityTests
{
    [Fact]
    public void FullName() =>
        Assert.Equal("Matilda Leana Cox", Badge.Label("Matilda", "Leana", "Cox"));

    [Fact]
    public void MissingMiddleName() =>
        Assert.Equal("Matilda Cox", Badge.Label("Matilda", null, "Cox"));

    [Fact]
    public void OnlyFirstName() =>
        Assert.Equal("Matilda", Badge.Label("Matilda", null, null));

    [Fact]
    public void OnlyLastName() =>
        Assert.Equal("Cox", Badge.Label(null, null, "Cox"));

    [Fact]
    public void FontSize() =>
        Assert.Equal(3*17, Badge.WidthInPixels("Cox", 17));

    [Fact]
    public void NothingToDisplay() =>
        Assert.Equal(0, Badge.WidthInPixels(null, 17));

    [Fact]
    public void NoFontSize() =>
        Assert.Equal(0, Badge.WidthInPixels("Matilda", null));

}
