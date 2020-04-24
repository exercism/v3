public class RemoteControlCar
{
    private int _battery;
    private int _distance;

    public RemoteControlCar()
    {
        _battery = 100;
        _distance = 0;
    }

    public void Drive()
    {
        if (_battery > 0)
        {
            _battery--;
            _distance += 20;
        }
    }

    public string DistanceDisplay()
    {
        return $"Driven {_distance} meters";
    }

    public string BatteryDisplay()
    {
        if (_battery == 0)
        {
            return "Battery empty";
        }

        return $"Battery at {_battery}%";
    }

    public static RemoteControlCar FromFriend()
    {
        var remoteControlCar = new RemoteControlCar();
        remoteControlCar.Drive();
        remoteControlCar.Drive();
        remoteControlCar.Drive();

        return remoteControlCar;
    }
}
