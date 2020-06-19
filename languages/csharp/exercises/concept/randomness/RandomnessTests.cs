using Xunit;
using System;

public class UserDefinedExceptionsTests
{
    [Fact]
    public void RollDie()
    {
        var player = new Player();
        var pips = player.RollDie();
        Assert.InRange(pips, 1, 18);
    }

    [Fact/*(Skip = "Remove this Skip property to run this test")*/]
    public void GenerateSpellStrength()
    {
        var player = new Player();
        var strength = player.GenerateSpellStrength();
        Assert.InRange(strength, 0.0, 100.0);
    }
}
