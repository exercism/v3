using System;
using Xunit;

public class ExpressionBodiedMembersTests
{
    [Fact]
    public void GetReading()
    {
        var ws = new WeatherStation();
        ws.AcceptReading(new Reading(20m, 25m, 0.01m));
        decimal[] expected = {20, 25, 0.01m};
        decimal[] actual = {ws.LatestTemperature, ws.LatestPressure, ws.LatestRainfall};
        Assert.Equal(expected, actual);
    }

    [Fact /*(Skip = "Remove this Skip property to run this test")*/]
    public void HasHistory_no()
    {
        var ws = new WeatherStation();
        ws.AcceptReading(new Reading(20m, 25m, 0.01m));
        Assert.False(ws.HasHistory);
    }

    [Fact /*(Skip = "Remove this Skip property to run this test")*/]
    public void HasHistory_yes()
    {
        var ws = new WeatherStation();
        ws.AcceptReading(new Reading(20m, 25m, 0.01m));
        ws.AcceptReading(new Reading(21m, 25m, 0.00m));
        Assert.True(ws.HasHistory);
    }

    [Fact /*(Skip = "Remove this Skip property to run this test")*/]
    public void ClearAll()
    {
        var ws = new WeatherStation();
        ws.AcceptReading(new Reading(20m, 25m, 0.01m));
        ws.AcceptReading(new Reading(21m, 25m, 0.00m));
        ws.ClearAll();
        object[] expected = {false, 0m};
        object[] actual = {ws.HasHistory, ws.LatestTemperature};
        Assert.Equal(expected, actual);
    }

    [Fact /*(Skip = "Remove this Skip property to run this test")*/]
    public void Outlook_exception()
    {
        var ws = new WeatherStation();
        Assert.Throws<ArgumentException>(() => ws.Outlook);
    }

    [Fact /*(Skip = "Remove this Skip property to run this test")*/]
    public void Outlook_cool()
    {
        var ws = new WeatherStation();
        ws.AcceptReading(new Reading(7m, 7m, 0m));
        Assert.Equal(Outlook.Cool, ws.Outlook);
    }

    [Fact /*(Skip = "Remove this Skip property to run this test")*/]
    public void Outlook_good()
    {
        var ws = new WeatherStation();
        ws.AcceptReading(new Reading(55m, 7m, 0m));
        Assert.Equal(Outlook.Good, ws.Outlook);
    }

    [Fact /*(Skip = "Remove this Skip property to run this test")*/]
    public void Outlook_warm()
    {
        var ws = new WeatherStation();
        ws.AcceptReading(new Reading(40m, 7m, 0m));
        Assert.Equal(Outlook.Warm, ws.Outlook);
    }

    [Fact /*(Skip = "Remove this Skip property to run this test")*/]
    public void RunSelfTest_good()
    {
        var ws = new WeatherStation();
        ws.AcceptReading(new Reading(40m, 7m, 0m));
        Assert.Equal(State.Good, ws.RunSelfTest());
    }

    [Fact /*(Skip = "Remove this Skip property to run this test")*/]
    public void RunSelfTest_bad()
    {
        var ws = new WeatherStation();
        Assert.Equal(State.Bad, ws.RunSelfTest());
    }
}
