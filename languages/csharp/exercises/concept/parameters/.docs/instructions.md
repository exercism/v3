The remote control car project you kicked off in the `classes` exercise has gone well (congratulations!) and due to a number of recent sponsorship deals there is money in the budget for enhancements.

Part of the budget is being used to provide some telemetry.

To keep the sponsors sweet a panel has been installed on the car to display the sponsors names as it goes round the track.

You will note that the introduction of these fancy new technologies has dramatically reduced the car's battery life.

## 1. Enable the remote control car to display sponsor names

Implement `AddSponsors` to take one or more sponsor names and store them on the car.

Note `AddSponsors` is guaranteed to be called at most once and the argument is guaranteed to be non-null.

Implement `DisplaySponsor` to display the selected sponsor. The first sponsor added has a `sponsorNum` of 0, the second, 1 etc.

```csharp
var car = RemoteControlCar.Buy();
car.AddSponsors("Exercism", "Walker Industries", "Acme Co.");
var sp2 = car.DisplaySponsor(sponsorNum: 1);
// => "Walker Industries"
```

## 2 Add functionality so that the telemetry system can get battery usage per meter

Implement `RemoteControlCar.GetTelemetryData()`.

`GetTelemetryData()` should make the battery percentage and distance driven in meters availble via `out` parameters.

`GetTelementryData()` should return `false` if the timestamp argument is less than the previously received value. (There is some issue of multiple telemtry nodes being involved).

Implement `TelemetryClient.GetBatteryUsagePerMeter()`.

This will call `RemoteControlCar.GetTelemetryData()`. If `GetTelemetryData()` returns `false` then this routine should return "no data". If `GetTelemetryData()` returns `true` then a message in the following form should be returned ""usage-per-meter=<BATTERY-USAGE-PER-METER". Where the calculation is (100 - current battery percentge) divided by the distance driven in meters so far.

```csharp
var car = RemoteControlCar.Buy();
car.Drive(); car.Drive();
var tc = new TelemetryClient(car);
tc.GetBatteryUsagePerMeter(timestamp: 3L);
// => "usage-per-meter=5"

tc.GetBatteryUsagePerMeter(timestamp: 1L);
// => "no data"
```

## Add a telemetry check that the remote control car is in good order

Implement `TelemetryClient.IsCarOk()`.

This will call `RemoteControlCar.GetTelemetryData()`. If `GetTelemetryData()` returns `false` then this routine should return "no data".

If `GetTelemetryData()` returns `true` then provided that battery usage is currently above 50% then the message "car ok" is returned by the telemetry client otherwise "car bad" is returned.

```csharp
var car = RemoteControlCar.Buy();
var tc = new TelemetryClient(car);
car.Drive(); car.Drive();
tc.IsCarOk(timestamp: 2L);
// => "car ok"

car.Drive(); car.Drive(); car.Drive(); car.Drive();
tc.IsCarOk(timestamp: 4L);
// => "car bad"
```
