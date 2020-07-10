using System;
using System.Collections.Generic;
using Xunit;

public class ObjectInitializationTests
{
    [Fact]
    public void GetAdmin()
    {
        var admin = new Identity {EyeColor = "green", Email = "admin@ex.ism"};
        var authenticator = new Authenticator(admin);
        Assert.Equal(admin, authenticator.GetAdmin());
    }

    [Fact /*(Skip = "Remove this Skip property to run this test")*/]
    public void GetDevelopers()
    {
        var authenticator = new Authenticator(new Identity {EyeColor = "green", Email = "admin@ex.ism"});
        var devs = authenticator.GetDevelopers() as IDictionary<string, Identity>;
        bool?[] actual = {devs != null, devs?.Count == 2, devs?["Anders"].EyeColor == "brown"};
        bool?[] expected = {true, true, true};
        Assert.Equal(expected, actual);
    }

    [Fact /*(Skip = "Remove this Skip property to run this test")*/]
    public void TamperWithDevelopers()
    {
        // @ErikSchierboom
        // the dictionary should be returned as a read-only collection to avoid its being
        // tampered with by the caller.
        //
        // However, I'm not sure this should be a test.
        // I can't leave the above explanation in as a) it gives too much away, b) the in-browser
        // student won't see it, so the student won't really understand what the problem is in the event
        // of the test failing.
        //
        // Should we rely just on the representations or analyzer and scrap this test?
        //
        var authenticator = new Authenticator(new Identity {EyeColor = "green", Email = "admin@ex.ism"});
        var devs = authenticator.GetDevelopers() as IDictionary<string, Identity>;
        bool?[] actual = {devs != null, devs?.IsReadOnly, devs?.Count == 2, devs?["Anders"].EyeColor == "brown"};
        bool?[] expected = {true, true, true, true};
        Assert.Equal(expected, actual);
    }
}
