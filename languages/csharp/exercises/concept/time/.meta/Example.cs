using System;

public enum Location
{
    NewYork,
    London,
    Paris
}

public enum AlertLevel
{
    Early,
    Standard,
    Late
}

public static class Appointment
{
    public static DateTime ShowLocalTime(DateTime dt)
    {
        return dt.ToLocalTime();
    }
    public static DateTime Schedule(string appointmentDateDescription, Location location)
    {
        string tzid = string.Empty;
        switch (location)
        {
            case Location.NewYork:
                tzid = "America/New_York";
                break;
            case Location.London:
                tzid = "Europe/London";
                break;
            case Location.Paris:
                tzid = "Europe/Paris";
                break;
        }
        DateTime dt = DateTime.Parse(appointmentDateDescription);
        DateTime local = TimeZoneInfo.ConvertTimeBySystemTimeZoneId(dt, tzid);
        return local;
    }

    public static DateTime GetAlertTime(DateTime appointment, AlertLevel alertLevel)
    {
        TimeSpan noticePeriod = new TimeSpan();

        switch (alertLevel)
        {
            case AlertLevel.Early:
                noticePeriod = new TimeSpan(1, 0, 0, 0);
                break;
            case AlertLevel.Standard:
                noticePeriod = new TimeSpan(1, 45, 0);
                break;
            case AlertLevel.Late:
                noticePeriod = new TimeSpan(0, 30, 0);
                break;
        }

        return appointment - noticePeriod;
    }

    public static bool HasDaylightSavingChanged(DateTime dt, Location location)
    {
        DateTime dtPrevious = dt.AddDays(-7);
        string tzid = string.Empty;
        switch (location)
        {
            case Location.NewYork:
                tzid = "America/New_York";
                break;
            case Location.London:
                tzid = "Europe/London";
                break;
            case Location.Paris:
                tzid = "Europe/Paris";
                break;
        }
        TimeZoneInfo tzi = TimeZoneInfo.FindSystemTimeZoneById(tzid);
        return tzi.IsDaylightSavingTime(dtPrevious) != tzi.IsDaylightSavingTime(dt);
    }
}
