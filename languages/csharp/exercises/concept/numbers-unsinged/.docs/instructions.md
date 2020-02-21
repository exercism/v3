# Instructions

In this exercise you'll be writing code to convert distance from meters into light minutes. A light second is precisely 299792458 meters. Calculations with meters and light seconds produce very large numbers quickly so we need to make sure that the numbers the program calculates can fit in the memory that is assigned for it. Distances at this scale also needs precision so light seconds should be defined with the most precise floating point type C# offers.

We also need to think about the hard limits of integral numeric types of the C# language. As you can imagine the distance to the sun in meters is a large number but the distance to Alpha Centauri[wiki-alpha_centauri] or beyond can become a number that is to large for these types.

### 1. Convert meters into light minutes

Implement a method that converts light minutes into meters.

```csharp
LightDistance.LightMinutesToMeters(149_597_870_700)
// Returns: 75.848726988322034438905064116m
```

### 2. Convert light minutes into meters

Implement a method that converts light minutes into meters. If the program reaches a memory limit it should return 0.

```csharp
LightDistance.LightMinutesToMeters(75.848726988322034438905064116m)
// Returns: 149597870700
```

[wiki-alpha_centauri]: https://en.wikipedia.org/wiki/Alpha_Centauri
