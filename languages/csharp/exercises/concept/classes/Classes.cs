public class Runner
{
    private int _stamina;
    private int _speed;
    private int _distanceCovered = 0;

    public Runner(int stamina, int speed)
    {
        _stamina = stamina;
        _speed = speed;
    }

    public void Run()
    {
        if (_stamina <= 0)
        {
            return;
        }

        _stamina -= 5;
        _distanceCovered += _speed;
    }

    public int DistanceCovered()
    {
        return _distanceCovered;
    }
}

public class Race
{
    private Runner _challenger;
    private Runner _recordHolder = RecordHolder();

    public Race(Runner challenger)
    {
        _challenger = challenger;
    }

    public void Run()
    {
        _challenger.Run();
        _recordHolder.Run();
    }

    public bool RecordBroken()
    {
        return _challenger.DistanceCovered() > _recordHolder.DistanceCovered();
    }

    public static Runner RecordHolder()
    {
        return new Runner(25, 15);
    }
}
