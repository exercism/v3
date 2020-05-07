In this exercise you're playing a role-playing game named "Wizard and Warriors," which allows you to play (unsurprisingly) as a wizard or a warrior.

The wizard and warrior have some common data and behavior:

- They have a number of hit points.
- They can attack, which reduces the number of hit points of the other character.
- If the number of hit points is less than or equal to zero, they are stunned and cannot do any more damage.

There are also some differences between the two character types.

|                    | Warriors          | Wizards             |
| ------------------ | ----------------- | ------------------- |
| Initial hit points | 30                | 20                  |
| Default attack     | 6                 | 3                   |
| Special attack     | 10 (potion drunk) | 12 (spell prepared) |

Drinking a potion (warrior) or preparing a spell (wizard) only makes the next attack a special attack. Subsequent attacks will be default attacks, unless a new potion is drunk or a new spell is prepared.

You have six tasks, each of which will work with remote controller car instances.

### 1. Display

To make it easy to check a character's remaining hit points:

```csharp
int speed = 5;
int batteryDrain = 2;
var car = new RemoteControlCar(speed, batteryDrain);
```

### 2. Creating a race track

Allow creating a race track by defining a constructor for the `RaceTrack` class that takes the track's distance in meters as its sole parameter (which is of type `int`):

```csharp
int distance = 800;
var raceTrack = new RaceTrack(distance);
```

### 3. Drive the car

Implement the `RemoteControlCar.Drive()` method that updates the number of meters driven based on the car's speed. Also implement the `RemoteControlCar.DistanceDriven()` method to return the number of meters driven by the car:

```csharp
int speed = 5;
int batteryDrain = 2;
var car = new RemoteControlCar(speed, batteryDrain);
car.Drive();

car.DistanceDriven();
// => 5
```

### 4. Check for a drained battery

Update the `RemoteControlCar.Drive()` method to drain the battery based on the car's battery drain. Also implement the `RemoteControlCar.BatteryDrained()` method that indicates if the battery is drained:

```csharp
int speed = 5;
int batteryDrain = 2;
var car = new RemoteControlCar(speed, batteryDrain);
car.Drive();

car.BatteryDrained();
// => false
```

### 5. Create the top of the line remote control car

The current top of the line car is the Nitro, which speed is a stunning 50 meters with a battery drain of 4%. Implement the (static) `RemoteControlCar.TopOfTheLine()` method to return this top of the line car:

```csharp
var car = RemoteControlCar.TopOfTheLine();
car.Drive();
car.DistanceDriven();
// => 50
```

### 6. Check if a remote control car can finish a race

To finish a race, a car has to be able to drive the race's distance. This means not draining its battery before having crossed the finish line. Implement the `Race.CarCanFinish()` method that takes a `RemoteControlCar` instance as its parameter and returns `true` if the car can finish the race; otherwise, return `false`:

```csharp
int speed = 5;
int batteryDrain = 2;
var car = new RemoteControlCar(speed, batteryDrain);

int distance = 100;
var race = new Race(distance);

race.CarCanFinish(car);
// => true
```
