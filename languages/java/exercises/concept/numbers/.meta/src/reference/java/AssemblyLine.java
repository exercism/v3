public class AssemblyLine {

    private final int DEFAULT_PRODUCTION_RATE = 221;

    public double productionRatePerHour(int speed)
    {
        return productionRatePerHourForSpeed(speed) * successRate(speed);
    }

    public int workingItemsPerMinute(int speed)
    {
        return (int) (productionRatePerHour(speed) / 60);
    }

    private int productionRatePerHourForSpeed(int speed)
    {
        return DEFAULT_PRODUCTION_RATE * speed;
    }

    private double successRate(int speed)
    {
        if (speed == 10)
        {
            return 0.77;
        }

        if (speed == 9)
        {
            return 0.8;
        }

        if (speed >= 5)
        {
            return 0.9;
        }

        return 1;
    }
}