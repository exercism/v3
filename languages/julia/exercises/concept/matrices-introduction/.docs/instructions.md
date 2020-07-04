In this exercise you're going to write some code to help you cook a brilliant lasagna from your favorite cooking book.

You have three tasks, all related to the time spent cooking the lasasgna.

## 1. Calculate the preparation time in minutes

Define the `preptime` function that takes the number of layers you added to the lasagna as an argument and returns how many minutes you spent preparing the lasagna, assuming each layer takes you 2 minutes to prepare.

```julia
julia> preptime(4)
8
```

## 2. Calculate the remaining oven time in minutes

According to the cooking book, lasagna needs to be in the oven for a total of 60 minutes.
Define the `remaining_time` function that takes the actual minutes the lasagna has been in the oven as a parameter and returns how many minutes the lasagna still has to remain in the oven.

```julia
julia> remaining_time(50)
10
```

## 3. Calculate the total working time in minutes

Define the `total_working_time` function that takes two arguments: the first argument is the number of layers you added to the lasagna, and the second parameter is the number of minutes the lasagna has been in the oven.
The function should return how many minutes in total you've worked on cooking the lasagna, which is the sum of the preparation time in minutes, and the time in minutes the lasagna has spent in the oven at the moment.

```julia
julia> total_working_time(3, 20)
26
```
