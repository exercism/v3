using Xunit;
using System;

public class RandomnessTests
{
    [Fact]
    public void RollDie()
    {
        var player = new Player();
        bool outOfRange = false;
        for (int i = 0; i < 100; i++)
        {
            var pips = player.RollDie();
            if (pips <= 0 || pips > 18)
            {
                outOfRange = true;
                break;
            }
        }
        Assert.False(outOfRange);
    }

    [Fact/*(Skip = "Remove this Skip property to run this test")*/]
    public void GenerateSpellStrength()
    {
        var player = new Player();
        var strength = player.GenerateSpellStrength();
        Assert.InRange(strength, 0.0, 100.0);
    }
}
