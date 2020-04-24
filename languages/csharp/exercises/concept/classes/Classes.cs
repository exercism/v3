namespace csharp
{
    public class RacingCircuitCar
    {
        private int _battery;
        private int _distance;
        private bool _turbo;

        public RacingCircuitCar()
        {
            _battery = 100;
            _distance = 0;
        }

        public void UseTurbo()
        {
            _turbo = true;
        }

        public void Drive()
        {
            if (Stopped())
            {
                return;
            }

            if (_turbo)
            {
                _battery -= 10;
                _distance += 7;
            }
            else
            {
                _battery -= 4;
                _distance += 3;
            }
        }

        public bool Stopped()
        {
            return _battery <= 0;
        }

        public bool Finished()
        {
            return _distance >= 50;
        }

        public static RacingCircuitCar Regular()
        {
            return new RacingCircuitCar();
        }

        public static RacingCircuitCar Turbo()
        {
            var car = new RacingCircuitCar();
            car.UseTurbo();

            return car;
        }
    }
}
