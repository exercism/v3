using System;
using Xunit;

public class NamespacesTests
{
    [Fact]
    public void Namespace_for_CarBuilder_is_Combined()
    {
        var carBuilderType = Type.GetType(GetNamespaceDotClass(CombinedNamespace, CarBuilderClass));
        Assert.NotNull(carBuilderType);
    }

    [Fact(Skip = "Remove this Skip property to run this test")]
    public void Namespace_for_CarBuilder_has_method_BuildBlue()
    {
        var carBuilderType = Type.GetType(GetNamespaceDotClass(CombinedNamespace, CarBuilderClass));
        Assert.NotNull(carBuilderType?.GetMethod(BuildBlueMethod));
    }

    [Fact(Skip = "Remove this Skip property to run this test")]
    public void Namespace_for_CarBuilder_has_method_BuildRed()
    {
        var carBuilderType = Type.GetType(GetNamespaceDotClass(CombinedNamespace, CarBuilderClass));
        Assert.NotNull(carBuilderType?.GetMethod(BuildRedMethod));
    }

    [Fact(Skip = "Remove this Skip property to run this test")]
    public void Namespace_for_CarBuilder_returns_Blue_Type()
    {
        var carBuilderType = Type.GetType(GetNamespaceDotClass(CombinedNamespace, CarBuilderClass));
        var returnType = carBuilderType?.GetMethod(BuildBlueMethod)?.ReturnType;
        Assert.Equal(GetNamespaceDotClass("BlueRemoteControlCarTeam", "RemoteControlCar"), returnType?.FullName);
    }

    [Fact(Skip = "Remove this Skip property to run this test")]
    public void Namespace_for_CarBuilder_returns_Red_Type()
    {
        var carBuilderType = Type.GetType(GetNamespaceDotClass(CombinedNamespace, CarBuilderClass));
        var returnType = carBuilderType?.GetMethod(BuildRedMethod)?.ReturnType;
        Assert.Equal(GetNamespaceDotClass("RedRemoteControlCarTeam", "RemoteControlCar"), returnType?.FullName);
    }

    [Fact(Skip = "Remove this Skip property to run this test")]
    public void Namespace_for_CarBuilder_can_BuildBlue_car()
    {
        var carBuilderType = Type.GetType(GetNamespaceDotClass(CombinedNamespace, CarBuilderClass));
        object blueRemoteControlCar = carBuilderType?.GetMethod(BuildBlueMethod)?.Invoke(null, null);
        Assert.NotNull(blueRemoteControlCar);
    }

    [Fact(Skip = "Remove this Skip property to run this test")]
    public void Namespace_for_CarBuilder_can_BuildRed_car()
    {
        var carBuilderType = Type.GetType(GetNamespaceDotClass(CombinedNamespace, CarBuilderClass));
        object redRemoteControlCar = carBuilderType?.GetMethod(BuildRedMethod)?.Invoke(null, null);
        Assert.NotNull(redRemoteControlCar);
    }

    private const string CombinedNamespace = "Combined";
    private const string CarBuilderClass = "CarBuilder";
    private const string BuildRedMethod = "BuildRed";
    private const string BuildBlueMethod = "BuildBlue";
    private string GetNamespaceDotClass(string @namespace, string @class) => $"{@namespace}.{@class}";
}
