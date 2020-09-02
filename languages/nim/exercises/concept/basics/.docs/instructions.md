In this exercise you're going to write some code to help you cook a brilliant lasagna from your favorite cooking book.

You have five tasks, all related to the time spent cooking the lasagna.

## 1. Define the expected oven time in minutes

Define the `expectedMinutesInOven` immutable variable that returns how many minutes the lasagna should be in the oven. It must be exported. According to the cooking book, the expected oven time in minutes is 40:

```nim
nim> expectedMinutesInOven
40 == type int
```

## 2. Calculate the remaining oven time in minutes

Implement the `remainingMinutesInOven` procedure that takes the actual minutes the lasagna has been in the oven as a parameter and returns how many minutes the lasagna still has to remain in the oven, based on the expected oven time in minutes from the previous task.

```nim
nim> remainingMinutesInOven(30)
10 == type int
```

## 3. Calculate the preparation time in minutes

Implement the `preparationTimeInMinutes` procedure that takes the number of layers you added to the lasagna as a parameter and returns how many minutes you spent preparing the lasagna, assuming each layer takes you 2 minutes to prepare.

```nim
nim> preparationTimeInMinutes(2)
4 == type int
```

## 4. Calculate the total working time in minutes

Implement the `totalTimeInMinutes` procedure that takes two parameters: the `numberOfLayers` parameter is the number of layers you added to the lasagna, and the `actualMinutesInOven` parameter is the number of minutes the lasagna has been in the oven. The procedure should return how many minutes in total you've worked on cooking the lasagna, which is the sum of the preparation time in minutes, and the time in minutes the lasagna has spent in the oven at the moment.

```nim
nim> totalTimeInMinutes(3, 20)
26 == type int
```

## 5. Update the recipe with notes

Go back through the recipe, adding notes and documentation.

```nim
proc totalTimeInMinutes*(numberOfLayers, actualMinutesInOven: int): int =
  ## Calculate the total working time. That is, the time to prepare all the layers
  ## of lasagna, and the time already spent in the oven.
```
