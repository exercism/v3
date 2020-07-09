using System;
using System.Collections.Generic;

public class WeatherStation
{
    private Reading reading;
    private List<DateTime> recordDates = new List<DateTime>();
    private List<decimal> temperatures = new List<decimal>();

    public void AcceptReading(Reading reading)
    {
        this.reading = reading;
        recordDates.Add(DateTime.Now);
        temperatures.Add(reading.Temperature);
    }

    public void ClearAll()
    {
        this.reading = new Reading();
        recordDates.Clear();
        temperatures.Clear();
    }

    public decimal LatestTemperature
    {
        get
        {
            return reading.Temperature;
        }
    }

    public decimal LatestPressure
    {
        get
        {
            return reading.Pressure;
        }
    }

    public decimal LatestRainfall
    {
        get
        {
            return reading.Rainfall;
        }
    }

    public bool HasHistory
    {
        get
        {
            if (recordDates.Count > 1)
            {
                return true;
            }
            else
            {
                return false;
            }
        }
    }

    public Outlook Outlook
    {
        get
        {
            if (reading.Equals(new Reading()))
            {
                throw new ArgumentException();
            }
            else
            {
                if (reading.Pressure < 10m && reading.Temperature < 30m)
                {
                    return Outlook.Cool;
                }
                else if (reading.Temperature > 50)
                {
                    return Outlook.Good;
                }
                else
                {
                    return Outlook.Warm;
                }
            }
        }
    }

    public State RunSelfTest()
    {
        if (reading.Equals(new Reading()))
        {
            return State.Bad;
        }
        else
        {
            return State.Good;
        }
    }
}

/*** Please do not modify this struct ***/
public struct Reading
{
    public decimal Temperature { get; }
    public decimal Pressure { get; }
    public decimal Rainfall { get; }

    public Reading(decimal temperature, decimal pressure, decimal rainfall)
    {
        Temperature = temperature;
        Pressure = pressure;
        Rainfall = rainfall;
    }
}

/*** Please do not modify this enum ***/
public enum State
{
    Good,
    Bad
}

/*** Please do not modify this enum ***/
public enum Outlook
{
    Cool,
    Rainy,
    Warm,
    Good
}
