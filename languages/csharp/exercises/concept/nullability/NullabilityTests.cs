using Xunit;

public class NullabilityTests
{
    [Fact]
    public void True() =>
        Assert.Null(null);
}
