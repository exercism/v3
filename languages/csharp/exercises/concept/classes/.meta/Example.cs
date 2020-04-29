class RemoteControlCar
{
    private int battery = 100;
    private int distance = 0;

    public void Drive()
    {
        if (battery > 0)
        {
            battery--;
            distance += 20;
        }
    }

    public string DistanceDisplay()
    {
        return $"Driven {distance} meters";
    }

    public string BatteryDisplay()
    {
        if (battery == 0)
        {
            return "Battery empty";
        }

        return $"Battery at {battery}%";
    }

    public static RemoteControlCar Buy()
    {
        return new RemoteControlCar();
    }
}
