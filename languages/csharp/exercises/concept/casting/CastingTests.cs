using System;
using Xunit;

public class CastingTests
{
    [Fact]
    public void DisplaySecurityPass_manager()
    {
        var lm = new SecurityPassMaker();
        Assert.Equal("Too Important for a Security Pass", lm.GetDisplayName(new Manager()));
    }

    [Fact /*(Skip = "Remove this Skip property to run this test")*/]
    public void DisplaySecurityPass_pysio()
    {
        var lm = new SecurityPassMaker();
        Assert.Equal("The Physio", lm.GetDisplayName(new Physio()));
    }

    [Fact /*(Skip = "Remove this Skip property to run this test")*/]
    public void DisplaySecurityPass_pysio_with_alert()
    {
        var lm = new SecurityPassMaker();
        Assert.Equal("The Physio", lm.GetDisplayName(new Physio()));
    }

    [Fact /*(Skip = "Remove this Skip property to run this test")*/]
    public void DisplaySecurityPass_security()
    {
        var lm = new SecurityPassMaker();
        Assert.Equal("Security Team Member Priority Personnel", lm.GetDisplayName(new Security()));
    }

    [Fact /*(Skip = "Remove this Skip property to run this test")*/]
    public void DisplaySecurityPass_security_junior()
    {
        var lm = new SecurityPassMaker();
        Assert.Equal("Security Junior", lm.GetDisplayName(new SecurityJunior()));
    }

    [Fact /*(Skip = "Remove this Skip property to run this test")*/]
    public void DisplaySecurityPass_security_police_liaison()
    {
        var lm = new SecurityPassMaker();
        Assert.Equal("Police Liaison Officer", lm.GetDisplayName(new PoliceLiaison()));
    }

    [Fact /*(Skip = "Remove this Skip property to run this test")*/]
    public void DisplaySecurityPass_security_intern()
    {
        var lm = new SecurityPassMaker();
        Assert.Equal("Security Intern", lm.GetDisplayName(new SecurityIntern()));
    }
}
