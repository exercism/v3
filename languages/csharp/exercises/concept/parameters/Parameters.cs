using System;

namespace template
{
    public class RemoteControlCar
    {
        private int batteryPercentage = 100;
        private int distanceDrivenInMeters = 0;

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
            throw new NotImplementedException("Please implement the RemoteControlCar.AddSponsors() method");
        }

        public string DisplaySponsor(int sponsorNum)
        {
            throw new NotImplementedException("Please implement the RemoteControlCar.DisplaySponsor() method");
        }

        public bool GetTelemetryData(ref long timestamp,
            out int batteryPercentage, out int distanceDrivenInMeters)
        {
            throw new NotImplementedException("Please implement the RemoteControlCar.GetTelemetryData() method");
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
            throw new NotImplementedException("Please implement the TelemetryClient.IsCarOk() method");
        }

        public string GetBatteryUsagePerMeter(long timestamp)
        {
            throw new NotImplementedException("Please implement the TelemetryClient.GetBatteryUsagePerMeter() method");
        }
    }
}
