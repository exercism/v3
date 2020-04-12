using Xunit;

public class NullabilityTests
{
    [Fact]
    public void FullName() =>
        Assert.Equal("[17] - Ryder Herbert - MARKETING", Badge.Label(17, "Ryder Herbert", "Marketing"));

    [Fact(Skip = "Remove this Skip property to run this test")]
    public void MissingDepartment() =>
        Assert.Equal("[59] - Bogdan Rosario - GUEST", Badge.Label(59, "Bogdan Rosario", null));

    [Fact(Skip = "Remove this Skip property to run this test")]
    public void MissingId() =>
        Assert.Equal("Bogdan Rosario - MARKETING", Badge.Label(null, "Bogdan Rosario", "Marketing"));

    [Fact(Skip = "Remove this Skip property to run this test")]
    public void PrintSingleLine() =>
        Assert.Equal("[23337] - Tilly Swift - SALES", Badge.PrintLabel("[23337] - Tilly Swift - SALES", 150));

    [Fact(Skip = "Remove this Skip property to run this test")]
    public void PrintTwoLines() =>
        Assert.Equal("[27] - Matilda \nCox - MARKETING", Badge.PrintLabel("[27] - Matilda Cox - MARKETING", 15));

    [Fact(Skip = "Remove this Skip property to run this test")]
    public void PrintThreeLines() =>
        Assert.Equal("[12] - Matilda \nLeanna Cox - EU\n MARKETING", Badge.PrintLabel("[12] - Matilda Leanna Cox - EU MARKETING", 15));
}
