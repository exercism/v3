using Xunit;

public class NullabilityTests
{
    [Fact]
    public void FullName() =>
        Assert.Equal("[17] - Matilda Cox - MARKETING", Badge.Label(17, "Matilda Cox", "Marketing"));

    [Fact]
    public void MissingDepartment() =>
        Assert.Equal("[17] - Matilda Cox", Badge.Label(17, "Matilda Cox", null));

    [Fact]
    public void PrintSingleLine() =>
        Assert.Equal("[17] - Matilda Cox - MARKETING", Badge.PrintLabel("[17] - Matilda Cox - MARKETING", 150));

    [Fact]
    public void PrintTwoLines() =>
        Assert.Equal("[17] - Matilda \nCox - MARKETING", Badge.PrintLabel("[17] - Matilda Cox - MARKETING", 15));

    [Fact]
    public void PrintThreeLines() =>
        Assert.Equal("[17] - Matilda \nLeanna Cox - EU\n MARKETING", Badge.PrintLabel("[17] - Matilda Leanna Cox - EU MARKETING", 15));

    [Fact]
    public void PrintNoMaxWidth() =>
        Assert.Equal("[17] - Matilda Cox - MARKETING", Badge.PrintLabel("[17] - Matilda Cox - MARKETING", null));

}
