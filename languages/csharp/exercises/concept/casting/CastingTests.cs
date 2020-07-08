using System;
using Xunit;

public class CastingTests
{
    [Fact]
    public void DisplayLaminate_manager()
    {
        var lm = new LaminateMaker(false);
        Assert.Equal("Too Important for a Laminate", lm.GetDisplayName(new Manager()));
    }

    [Fact /*(Skip = "Remove this Skip property to run this test")*/]
    public void DisplayLaminate_pysio()
    {
        var lm = new LaminateMaker(false);
        Assert.Equal("The Physio", lm.GetDisplayName(new Physio()));
    }

    [Fact /*(Skip = "Remove this Skip property to run this test")*/]
    public void DisplayLaminate_pysio_with_alert()
    {
        var lm = new LaminateMaker(true);
        Assert.Equal("The Physio", lm.GetDisplayName(new Physio()));
    }

    [Fact /*(Skip = "Remove this Skip property to run this test")*/]
    public void DisplayLaminate_security()
    {
        var lm = new LaminateMaker(false);
        Assert.Equal("Security Team Member", lm.GetDisplayName(new Security()));
    }

    [Fact /*(Skip = "Remove this Skip property to run this test")*/]
    public void DisplayLaminate_security_with_alert()
    {
        var lm = new LaminateMaker(true);
        Assert.Equal("Security Team Member Priority Personnel", lm.GetDisplayName(new Security()));
    }

    [Fact /*(Skip = "Remove this Skip property to run this test")*/]
    public void DisplayLaminate_security_junior()
    {
        var lm = new LaminateMaker(true);
        Assert.Equal("Security Junior", lm.GetDisplayName(new SecurityJunior()));
    }

    [Fact /*(Skip = "Remove this Skip property to run this test")*/]
    public void DisplayLaminate_security_police_liaison()
    {
        var lm = new LaminateMaker(true);
        Assert.Equal("Police Liaison Officer", lm.GetDisplayName(new PoliceLiaison()));
    }

    [Fact /*(Skip = "Remove this Skip property to run this test")*/]
    public void DisplayLaminate_security_intern()
    {
        var lm = new LaminateMaker(false);
        Assert.Equal("Security Intern", lm.GetDisplayName(new SecurityIntern()));
    }

    [Fact /*(Skip = "Remove this Skip property to run this test")*/]
    public void ConvertShirtNum_good()
    {
        var lm = new LaminateMaker(false);
        Assert.Equal(21u, lm.ConvertShirtNum(21ul));
    }

    [Fact /*(Skip = "Remove this Skip property to run this test")*/]
    public void ConvertShirtNum_bad()
    {
        var lm = new LaminateMaker(false);
        Assert.Equal(0u, lm.ConvertShirtNum((ulong)uint.MaxValue + 10));
    }
}
