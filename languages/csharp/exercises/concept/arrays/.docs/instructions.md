You're an avid bird watcher that keeps track of how many birds have visited your garden in the last seven days.

You have five tasks, all dealing with the numbers of birds that visited your garden.

### 1. Calculate the total number of visiting birds

Implement the `BirdCount.Total()` method to return the total number of birds that have visited your garden:

```csharp
int[] birdsPerDay = { 2, 5, 1, 4, 7, 0 };
var birdCount = new BirdCount(birdsPerDay);
birdCount.Total();
// => 19
```

### 2. Calculate the number of busy days

Some days are busier that others. A busy day is one where five or more birds have visited your garden.
Implement the `BirdCount.BusyDays()` method to return the number of busy days:

```csharp
int[] birdsPerDay = { 2, 5, 1, 4, 7, 0 };
var birdCount = new BirdCount(birdsPerDay);
birdCount.BusyDays();
// => 2
```

### 3. Check how many birds visited yesterday

Implement the `BirdCount.Yesterday()` method to return how many birds visited your garden yesterday, which is the second last element:

```csharp
int[] birdsPerDay = { 2, 4, 1, 6, 7, 0 };
var birdCount = new BirdCount(birdsPerDay);
birdCount.Yesterday();
// => 7
```

### 4. Check the count for a specific day

Your neighbor is also a bird enthusiast. Occasionally, your neighbor wants to know if your garden was also visited by a specific number of birds. Implement the `BirdCount.Exact()` method that the number of birds to check as a parameter and returns a boolean that indicates if that exact number of birds has visited your garden:

```csharp
int[] birdsPerDay = { 2, 4, 1, 6, 7, 0 };
var birdCount = new BirdCount(birdsPerDay);
birdCount.Exact(1);
// => true
```

### 5. Check what the counts were last week

For comparison purposes, you always keep a copy of last week's counts nearby, which were: 0, 2, 5, 3, 7, 8 and 4. Implement the `BirdCount.LastWeek()` method that returns last week's counts:

```csharp
BirdCount.LastWeek();
// => [0, 2, 5, 3, 7, 8, 4]
```
