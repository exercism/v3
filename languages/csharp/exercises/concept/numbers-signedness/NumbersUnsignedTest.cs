using Xunit;

public class AssemblyLineTest
{
    [Fact]
    public void LightSecondsFromEarthToSun() =>
        Assert.Equal(499.0047838361564119134711521m, LightDistance.MetersToLightSeconds(meters: 149_597_870_700));

    [Fact]
    public void MetersFromEarthToSun() =>
        Assert.Equal((ulong)149_597_870_700, LightDistance.LightSecondsToMeters(lightMinutes: 499.0047838361564119134711521m));

    [Fact]
    public void LightSecondsFromMarsToSun() =>
        Assert.Equal(75.848726988322034438905064116m, LightDistance.MetersToLightSeconds(meters: 22_738_876_300));

    [Fact]
    public void MetersFromMarsToSun() =>
        Assert.Equal((ulong)22_738_876_300, LightDistance.LightSecondsToMeters(lightMinutes: 75.848726988322034438905064116m));

    [Fact]
    public void Overflow() =>
        Assert.Equal((ulong)0, LightDistance.LightSecondsToMeters(lightMinutes: 100_000_000_000m));
}