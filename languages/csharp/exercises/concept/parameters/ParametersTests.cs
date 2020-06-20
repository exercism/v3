using Xunit;

public class ParametersTests
{

    //    [Fact]
    public void DisplayNextSponsor_for_3_sponsors()
    {
        var car = RemoteControlCar.Buy();
        car.AddSponsors("Exercism", "Walker Industries", "Acme Co.");
        var sp1 = car.DisplaySponsor(sponsorNum: 0);
        var sp2 = car.DisplaySponsor(sponsorNum: 1);
        var sp3 = car.DisplaySponsor(sponsorNum: 2);
        Assert.Equal((sp1, sp2, sp3), ("Exercism", "Walker Industries", "Acme Co."));
    }

    [Fact/*(Skip = "Remove this Skip property to run this test")*/]
    public void GetTelmetryData()
    {
        var car = RemoteControlCar.Buy();
        car.Drive();
        car.Drive();
        long timestamp = 1L;
        car.GetTelemetryData(ref timestamp, out int batteryPercentage, out int distanceDrivenInMeters);
        Assert.Equal((80, 4), (batteryPercentage, distanceDrivenInMeters));
    }

    [Fact/*(Skip = "Remove this Skip property to run this test")*/]
    public void IsCarOK_Ok()
    {
        var tc = new TelemetryClient(RemoteControlCar.Buy());
        Assert.Equal("car ok", tc.IsCarOk(timestamp: 1L));
    }

    [Fact/*(Skip = "Remove this Skip property to run this test")*/]
    public void IsCarOK_ok()
    {
        var tc = new TelemetryClient(RemoteControlCar.Buy());
        Assert.Equal("car ok", tc.IsCarOk(timestamp: 1L));
    }

    [Fact/*(Skip = "Remove this Skip property to run this test")*/]
    public void IsCarOK_bad()
    {
        var car = RemoteControlCar.Buy();
        car.Drive(); car.Drive(); car.Drive(); car.Drive(); car.Drive(); car.Drive();
        var tc = new TelemetryClient(car);
        Assert.Equal("car bad", tc.IsCarOk(timestamp: 1L));
    }

    [Fact/*(Skip = "Remove this Skip property to run this test")*/]
    public void IsCarOK_no_data()
    {
        var tc = new TelemetryClient(RemoteControlCar.Buy());
        tc.IsCarOk(3L);
        Assert.Equal("no data", tc.IsCarOk(timestamp: 1L));
    }

    [Fact/*(Skip = "Remove this Skip property to run this test")*/]
    public void GetUsagePerMeter_good()
    {
        var car = RemoteControlCar.Buy();
        car.Drive(); car.Drive();
        var tc = new TelemetryClient(car);
        Assert.Equal("usage-per-meter=5", tc.GetBatteryUsagePerMeter(timestamp: 1L));
    }

    [Fact/*(Skip = "Remove this Skip property to run this test")*/]
    public void GetUsagePerMeter_not_started()
    {
        var car = RemoteControlCar.Buy();
        var tc = new TelemetryClient(car);
        Assert.Equal("no data", tc.GetBatteryUsagePerMeter(timestamp: 1L));
    }

}
