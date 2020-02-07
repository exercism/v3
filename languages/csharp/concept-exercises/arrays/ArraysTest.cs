using System;
using Xunit;

public static class ArraysTest
{
    public static int[] EmployeeHoursPerDaySorted() => new[]{9, 9, 8, 8, 8, 4, 0, 0};
    
    [Fact]
    public static void NoInputSum()
    {
       Assert.Equal(0, Arrays.SumOfDays()); 
    }

    [Fact]
    public static void NoInputAvg()
    {
        Assert.Equal(0, Arrays.AvgOfDays());    
    }

    [Fact]
    public static void NoFridayHours()
    {
        Assert.Equal(0, Arrays.FindFridayHours());     
    }
    
    [Fact]
    public static void NoInputSort()
    {
        Assert.Empty(Arrays.SortedDays());
    }

    [Fact]
    public static void SumTest()
    {
        Assert.Equal(46, Arrays.SumOfDays(Arrays.EmployeeHoursPerDay()));
    }

    [Fact]
    public static void AvgTest()
    {
        Assert.Equal(5.75, Arrays.AvgOfDays(Arrays.EmployeeHoursPerDay()));
    }

    [Fact]
    public static void FridayHours()
    {
        Assert.Equal(8, Arrays.FindFridayHours(Arrays.EmployeeHoursPerDay()));
    }

    [Fact]
    public static void SortHours()
    {
        Assert.Equal(EmployeeHoursPerDaySorted(),Arrays.SortedDays(Arrays.EmployeeHoursPerDay()));
    }
}
