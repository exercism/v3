public static class LightDistance
{
    public const int LightSecondInMeters = 299_792_458;

    public static decimal MetersToLightSeconds(ulong meters)
    {
        return (decimal)meters / LightSecondInMeters;
    }

    public static ulong LightSecondsToMeters(decimal lightMinutes)
    {
        try
        {
            return Convert.ToUInt64(lightMinutes * LightSecondInMeters);
        }
        catch (OverflowException)
        {
            return 0;
        }
    }
}