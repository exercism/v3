using Xunit;

public class InterfacesTests
{
    [Fact]
    public void Race()
    {
        var productionCar = new ProductionRemoteControlCar();
        var experimentalCar = new ExperimentalRemoteControlCar();
        TestTrack.Race((IRemoteControlCar)productionCar);
        TestTrack.Race((IRemoteControlCar)productionCar);
        TestTrack.Race((IRemoteControlCar)experimentalCar);
        TestTrack.Race((IRemoteControlCar)experimentalCar);
        Assert.Equal(20, experimentalCar.DistanceTravelled - productionCar.DistanceTravelled);
    }

    [Fact/*(Skip = "Remove this Skip property to run this test")*/]
    public void RankCars()
    {
        var prc1 = new ProductionRemoteControlCar();
        var prc2 = new ProductionRemoteControlCar();
        prc1.NumberOfVictories = 3;
        prc2.NumberOfVictories = 2;
        var rankings = TestTrack.GetRankedCars(prc1, prc2);
        Assert.Same(prc1, rankings[1]);
    }
}
