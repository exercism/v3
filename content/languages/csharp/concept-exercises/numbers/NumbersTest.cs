using Xunit;

public class AssemblyLineTest
{
    [Theory]
    [InlineData(0, 0.0)]
    [InlineData(1, 221.0)]
    [InlineData(2, 442.0)]
    [InlineData(3, 663.0)]
    [InlineData(4, 884.0)]
    [InlineData(5, 994.5)]
    [InlineData(6, 1193.4)]
    [InlineData(7, 1392.3)]
    [InlineData(8, 1591.2)]
    [InlineData(9, 1531.53)]
    [InlineData(10, 1701.7)]
    public void ProductionRatePerHour(int speed, double expected) =>
        Assert.Equal(expected, AssemblyLine.ProductionRatePerHour(speed));

    [Theory]
    [InlineData(0, 0)]
    [InlineData(1, 3)]
    [InlineData(2, 7)]
    [InlineData(3, 11)]
    [InlineData(4, 14)]
    [InlineData(5, 16)]
    [InlineData(6, 19)]
    [InlineData(7, 23)]
    [InlineData(8, 26)]
    [InlineData(9, 25)]
    [InlineData(10, 28)]
    public void WorkingItemsPerMinute(int speed, int expected) =>
        Assert.Equal(expected, AssemblyLine.WorkingItemsPerMinute(speed));
}