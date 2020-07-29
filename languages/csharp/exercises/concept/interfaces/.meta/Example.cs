using System;
using System.Collections.Generic;

public interface IRemoteControlCar
{
    void Drive();
    decimal DistanceTravelled { get; }
}

public class ProductionRemoteControlCar : IRemoteControlCar, IComparable<ProductionRemoteControlCar>
{
    public decimal DistanceTravelled { get; private set; }
    public int NumberOfVictories { get; set; }

    public void Drive()
    {
        DistanceTravelled += 10;
    }

    public int CompareTo(ProductionRemoteControlCar other)
    {
        if (ReferenceEquals(this, other)) return 0;
        if (ReferenceEquals(null, other)) return 1;
        return NumberOfVictories.CompareTo(other.NumberOfVictories);
    }
}

public class ExperimentalRemoteControlCar : IRemoteControlCar
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
        car.Drive();
        return car.DistanceTravelled;
    }

    public static void RankCars(List<ProductionRemoteControlCar> unrankedCars)
    {
        unrankedCars.Sort();
    }
}
