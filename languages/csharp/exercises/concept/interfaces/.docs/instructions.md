In this exercise you will be doing some more work on the remote control cars.

An experimental car has been developed and the test track needs to be adapted to handle both production and experimental models. The two types of car have already been built and you need to find a way to deal with them both on the test track.

In addition, production cars are beginning to have some success. The team boss is keen to maintain the competitive spirit by publishing a ranking of the production cars.

## 1. Enable cars to be driven on the same test track

Please add a method to the `IRemoteControlCar` to expose the implementations of `Drive()` for the two types of car.

```csharp
TestTrack.Race(new ProductionRemoteCcontrolCar());
TestTrack.Race(new ExperimentalRemoteCcontrolCar());
// this should execute without an exception
```

## 2. Enable the distance travelled on the test track to be compared

Please add a property to the `IRemoteControlCar` to expose the implementations of `DistanceTravelled` for the two types of car.

```csharp
var prod = new ProductionRemoteCcontrolCar();
TestTrack.Race(prod);
var exp = new ExperimentalRemoteCcontrolCar();
TestTrack.Race(exp);
prod.DistanceTravelled
// => 10
exp.DistanceTravelled
// => 20
```

## 3. Allow the production cars to be ranked

Please implement the `IComparable<T>` interface in the `ProductionControlCar` class.

```csharp
var prc1 = new ProductionRemoteControlCar();
var prc2 = new ProductionRemoteControlCar();
prc1.NumberOfVictories = 3;
prc2.NumberOfVictories = 2;
var rankings = TestTrack.GetRankedCars(prc1, prc2);
// => rankings[1] == prc1
```
