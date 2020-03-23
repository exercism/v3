using System;
using Xunit;


public class WeighingMachineTests
{
    [Fact]
    public void Got_weight_is_set_weight()
    {
        var wm = new WeighingMachine();
        wm.Weight = 77.7f;
        Assert.Equal(77.7, wm.Weight, 3 );
    }
    [Fact( Skip = "Remove to run test")]
    public void Got_british_weight()
    {
        var wm = new WeighingMachine();
        wm.Weight = 77.7f;
        var expected = new BritishWeight(12, 3, 4);
        Assert.Equal(expected, wm.BritishWeight );
    }
    [Fact( Skip = "Remove to run test")]
    public void Got_british_weight_in_pounds()
    {
        var wm = new WeighingMachine();
        wm.Units = Units.Pounds;
        wm.Weight = 175f;
        Assert.Equal((12, 7, 0), (wm.BritishWeight.Stones, wm.BritishWeight.Pounds, wm.BritishWeight.Ounces) );
    }
    [Fact( Skip = "Remove to run test")]
    public void Got_reduced_weight()
    {
        var wm = new WeighingMachine();
        wm.Weight = 100;
        wm.Reduction = 10;
        Assert.Equal(90, wm.Weight );
    }
    [Fact( Skip = "Remove to run test")]
    public void Negative_weight_is_invalid()
    {
        var wm = new WeighingMachine();
        Assert.Throws<ArgumentException>(() => wm.Weight = -10);
    }

}