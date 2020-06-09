using Xunit;

public class TuplesTest
{
    [Fact/*(Skip = "Remove this Skip property to run this test")*/]
    public void Analyze_non_fake_non_newyork()
    {
        Assert.Equal((false, false, "1234"), PhoneNumbers.Analyze("631-502-1234"));
    }

    [Fact/*(Skip = "Remove this Skip property to run this test")*/]
    public void Analyze_fake_non_newyork()
    {
        Assert.Equal((false, true, "1234"), PhoneNumbers.Analyze("631-555-1234"));
    }

    [Fact/*(Skip = "Remove this Skip property to run this test")*/]
    public void Analyze_non_fake_newyork()
    {
        Assert.Equal((true, false, "1234"), PhoneNumbers.Analyze("212-502-1234"));
    }

    [Fact/*(Skip = "Remove this Skip property to run this test")*/]
    public void Analyze_fake_newyork()
    {
        Assert.Equal((true, true, "1234"), PhoneNumbers.Analyze("212-555-1234"));
    }

    [Fact/*(Skip = "Remove this Skip property to run this test")*/]
    public void Analyze_fake_fake()
    {
        Assert.Equal((false, false, "1234"), PhoneNumbers.Analyze("515-212-1234"));
    }

    [Fact/*(Skip = "Remove this Skip property to run this test")*/]
    public void Is_Fake()
    {
        Assert.True(PhoneNumbers.IsFake(PhoneNumbers.Analyze("212-555-1234")));
    }
}
