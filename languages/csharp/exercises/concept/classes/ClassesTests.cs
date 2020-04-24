using Xunit;

public class RacingCircuitCarTests
{
    [Fact]
    public void CarFromFriendBatteryDisplay()
    {
        var car = RemoteControlCar.FromFriend();
        Assert.Equal("Battery at 97%", car.BatteryDisplay());
    }
    
    [Fact]
    public void CarFromFriendDistanceDisplay()
    {
        var car = RemoteControlCar.FromFriend();
        Assert.Equal("Driven 60 meters", car.DistanceDisplay());
    }
    
    [Fact]
    public void NewCarBatteryDisplay()
    {
        var car = new RemoteControlCar();
        Assert.Equal("Battery at 100%", car.BatteryDisplay());
    }
    
    [Fact]
    public void NewCarDistanceDisplay()
    {
        var car = new RemoteControlCar();
        Assert.Equal("Driven 0 meters", car.DistanceDisplay());
    }
    
    [Fact]
    public void BatteryDisplayAfterDrivingOnce()
    {
        var car = new RemoteControlCar();
        car.Drive();
        Assert.Equal("Battery at 99%", car.BatteryDisplay());
    }
    
    [Fact]
    public void DistanceDisplayAfterDrivingOnce()
    {
        var car = new RemoteControlCar();
        car.Drive();
        Assert.Equal("Driven 20 meters", car.DistanceDisplay());
    }
    
    [Fact]
    public void BatteryDisplayAfterDrivingMultipleTimes()
    {
        var car = new RemoteControlCar();

        for (var i = 0; i < 23; i++)
        {
            car.Drive();
        }
        
        Assert.Equal("Battery at 77%", car.BatteryDisplay());
    }
    
    [Fact]
    public void DistanceDisplayAfterDrivingMultipleTimes()
    {
        var car = new RemoteControlCar();

        for (var i = 0; i < 17; i++)
        {
            car.Drive();
        }

        Assert.Equal("Driven 340 meters", car.DistanceDisplay());
    }
    
    [Fact]
    public void BatteryDisplayWhenBatteryEmpty()
    {
        var car = new RemoteControlCar();

        for (var i = 0; i < 100; i++)
        {
            car.Drive();
        }
        
        Assert.Equal("Battery empty", car.BatteryDisplay());
    }
    
    [Fact]
    public void DistanceDisplayWhenBatteryEmpty()
    {
        var car = new RemoteControlCar();

        for (var i = 0; i < 100; i++)
        {
            car.Drive();
        }
        
        Assert.Equal("Driven 2000 meters", car.DistanceDisplay());
    }
}
