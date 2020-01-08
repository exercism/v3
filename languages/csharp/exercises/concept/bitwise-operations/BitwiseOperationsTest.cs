using Xunit;

// TODO: convert Theory-based tests to Fact-based tests.
// This is necessary in order to be able to display the 
// input for which the test failed, which is defined in
// the .meta/config.json file
public class PermissionsTest
{
    [Theory]
    [InlineData(AccountType.Guest, Permission.Read)]
    [InlineData(AccountType.User, Permission.Read | Permission.Write)]
    [InlineData(AccountType.Moderator, Permission.Read | Permission.Write | Permission.Delete)]
    public void Default(AccountType accountType, Permission expected) =>
        Assert.Equal(expected, Permissions.Default(accountType));

    [Theory]
    [InlineData((AccountType)123)]
    [InlineData((AccountType)7)]
    public void DefaultForUnknownAccountType(AccountType accountType) =>
        Assert.Equal(Permission.None, Permissions.Default(accountType));

    [Theory]
    [InlineData(Permission.None, Permission.Read, Permission.Read)]
    [InlineData(Permission.Read, Permission.Read, Permission.Read)]
    [InlineData(Permission.Read, Permission.None, Permission.Read)]
    [InlineData(Permission.Write, Permission.Read, Permission.Write | Permission.Read)]
    [InlineData(Permission.Delete, Permission.Read, Permission.Delete | Permission.Read)]
    [InlineData(Permission.Read | Permission.Write, Permission.Delete, Permission.Read | Permission.Write | Permission.Delete)]
    [InlineData(Permission.All, Permission.None, Permission.All)]
    [InlineData(Permission.All, Permission.Read, Permission.All)]
    public void Grant(Permission permission, Permission permissionToGrant, Permission expected) =>
        Assert.Equal(expected, Permissions.Grant(permission, permissionToGrant));

    [Theory]
    [InlineData(Permission.None, Permission.Read, Permission.None)]
    [InlineData(Permission.Read, Permission.Write, Permission.Read)]
    [InlineData(Permission.Read, Permission.All, Permission.None)]
    [InlineData(Permission.Write, Permission.None, Permission.Write)]
    [InlineData(Permission.Delete, Permission.Delete, Permission.None)]
    [InlineData(Permission.Read | Permission.Write, Permission.Write, Permission.Read)]
    [InlineData(Permission.All, Permission.Delete, Permission.Read | Permission.Write)]
    [InlineData(Permission.All, Permission.Read | Permission.Write, Permission.Delete)]
    public void Revoke(Permission permission, Permission permissionToRevoke, Permission expected) =>
        Assert.Equal(expected, Permissions.Revoke(permission, permissionToRevoke));

    [Theory]
    [InlineData(Permission.None, Permission.Read, false)]
    [InlineData(Permission.None, Permission.Write, false)]
    [InlineData(Permission.None, Permission.Delete, false)]
    [InlineData(Permission.None, Permission.All, false)]
    [InlineData(Permission.All, Permission.Read, true)]
    [InlineData(Permission.All, Permission.Write, true)]
    [InlineData(Permission.All, Permission.Delete, true)]
    [InlineData(Permission.All, Permission.All, true)]
    [InlineData(Permission.Read, Permission.Read, true)]
    [InlineData(Permission.Write, Permission.Write, true)]
    [InlineData(Permission.Delete, Permission.Delete, true)]
    [InlineData(Permission.Read | Permission.Write, Permission.Read, true)]
    [InlineData(Permission.Read | Permission.Write, Permission.Write, true)]
    [InlineData(Permission.Read | Permission.Write, Permission.Delete, false)]
    [InlineData(Permission.Read | Permission.Write, Permission.All, false)]
    [InlineData(Permission.Write | Permission.Delete, Permission.Read, false)]
    [InlineData(Permission.Write | Permission.Delete, Permission.Write, true)]
    [InlineData(Permission.Delete | Permission.Read, Permission.Delete, true)]
    public void Check(Permission owned, Permission desired, bool expected) =>
        Assert.Equal(expected, Permissions.Check(owned, desired));
}