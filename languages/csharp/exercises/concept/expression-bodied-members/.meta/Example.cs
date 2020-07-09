public class SpaceAge
{
    private const double EarthSeconds = 31557600;
    private readonly double seconds;

    public SpaceAge(long seconds)
    {
        this.seconds = seconds;
    }

    public double OnEarth() => PlanetSeconds(1d);

    public double OnMercury() => PlanetSeconds(0.2408467);

    public double OnVenus() => PlanetSeconds(0.61519726);

    public double OnMars() => PlanetSeconds(1.8808158);

    public double OnJupiter() => PlanetSeconds(11.862615);

    public double OnSaturn() => PlanetSeconds(29.447498);

    public double OnUranus() => PlanetSeconds(84.016846);

    public double OnNeptune() => PlanetSeconds(164.79132);

    private double PlanetSeconds(double planetFactor) => seconds / (EarthSeconds * planetFactor);
}
