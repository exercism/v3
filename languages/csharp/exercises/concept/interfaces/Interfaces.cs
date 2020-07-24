using System;
using System.Collections.Generic;

public interface IRemoteControlCar
{
    // TODO implement the IRemoteControlCar interface
}

public class ProductionRemoteControlCar
{
    public decimal DistanceTravelled { get; private set; }
    public int NumberOfVictories { get; set; }

    public void Drive()
    {
        DistanceTravelled += 10;
    }
}

public class ExperimentalRemoteControlCar
{
    public decimal DistanceTravelled { get; private set; }

    public void Drive()
    {
        DistanceTravelled += 20;
    }
}

public static class TestTrack
{
    public static decimal Race(IRemoteControlCar car)
    {
        throw new NotImplementedException($"Please implement the (static) TestTrack.Race() method");
    }

    public static List<ProductionRemoteControlCar> GetRankedCars(ProductionRemoteControlCar prc1,
        ProductionRemoteControlCar prc2)
    {
        throw new NotImplementedException($"Please implement the (static) TestTrack.GetRankedCars() method");
    }
}
