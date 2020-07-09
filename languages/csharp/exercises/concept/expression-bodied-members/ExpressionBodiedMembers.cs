public class SpaceAge
{
    private const double EarthSeconds = 31557600;
    private const double MercurySeconds = EarthSeconds * 0.2408467;
    private const double VenusSeconds = EarthSeconds * 0.61519726;
    private const double MarsSeconds = EarthSeconds * 1.8808158;
    private const double JupiterSeconds = EarthSeconds * 11.862615;
    private const double SaturnSeconds = EarthSeconds * 29.447498;
    private const double UranusSeconds = EarthSeconds * 84.016846;
    private const double NeptuneSeconds = EarthSeconds * 164.79132;
    private readonly double seconds;
    public SpaceAge(long seconds)
    {
        this.seconds = seconds;
    }

    public double OnEarth()
    {
        return seconds / EarthSeconds;
    }

    public double OnMercury()
    {
        return seconds / MercurySeconds;
    }

    public double OnVenus()
    {
        return seconds / VenusSeconds;
    }

    public double OnMars()
    {
        return seconds / MarsSeconds;
    }

    public double OnJupiter()
    {
        return seconds / JupiterSeconds;
    }

    public double OnSaturn()
    {
        return seconds / SaturnSeconds;
    }

    public double OnUranus()
    {
        return seconds / UranusSeconds;
    }

    public double OnNeptune()
    {
        return seconds / NeptuneSeconds;
    }
}
