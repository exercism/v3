using System;

public class RemoteControlCar
{
    private int batteryPercentage = 100;
    private int distanceDrivenInMeters = 0;
    private string[] sponsors = new string[0];
    private long latestTimestamp = 0L;
    public void Drive()
    {
        if (batteryPercentage > 0)
        {
            batteryPercentage -= 10;
            distanceDrivenInMeters += 2;
        }
    }

    public void AddSponsors(params string[] sponsors)
    {
        this.sponsors = sponsors;
    }

    public string DisplaySponsor(int sponsorNum)
    {
        return sponsors[sponsorNum];
    }

    public bool GetTelemetryData(ref long timestamp,
        out int batteryPercentage, out int distanceDrivenInMeters)
    {
        if (timestamp > latestTimestamp)
        {
            latestTimestamp = timestamp;
            batteryPercentage = this.batteryPercentage;
            distanceDrivenInMeters = this.distanceDrivenInMeters;
            return true;
        }
        else
        {
            latestTimestamp = timestamp;
            batteryPercentage = -1;
            distanceDrivenInMeters = -1;
            return false;
        }
    }

    public static RemoteControlCar Buy()
    {
        return new RemoteControlCar();
    }
}

public class TelemetryClient
{
    private RemoteControlCar car;

    public TelemetryClient(RemoteControlCar car)
    {
        this.car = car;
    }

    public string IsCarOk(long timestamp)
    {
        long localTime = timestamp;
        bool goodData = car.GetTelemetryData(ref localTime, out int batteryPercentage, out _);
        if (!goodData)
        {
            return "no data";
        }
        if (batteryPercentage > 50)
        {
            return "car ok";
        }

        return "car bad";
    }

    public string GetBatteryUsagePerMeter(long timestamp)
    {
        long localTime = timestamp;
        bool goodData = car.GetTelemetryData(ref localTime,
            out int batteryPercentage, out int distanceDrivenInMeters);
        if (!goodData || distanceDrivenInMeters == 0)
        {
            return "no data";
        }

        return "usage-per-meter=" + (100 - batteryPercentage) / distanceDrivenInMeters;
    }
}
