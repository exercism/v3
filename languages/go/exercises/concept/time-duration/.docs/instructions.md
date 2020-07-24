In this exercise you'll be working on an class room time schdule for a school. 

You have five tasks, which will all involve scheduling of class periods and intervals.The class duration is 1 hr in all the tasks given blow.

## 1. Parse the Duration of the Class

Implement the `StartTime` function to parse a textual representation of a time duration of the class into the corresponding `time.Duration` format and print the time:

```go
StartTime("1h")
// => 1h0m0s
```

## 2. Check if the class is over

Implement the `IsOver` function that takes the start time and a given time and finds out whether the current class is over or not :

```go
IsOver("7/25/2019 13:45:00","July 25, 2019 14:46:00")
// => true
```

## 3. Find out the time the class has exceeded

Implement the `ExtraTime` function that takes a time and finds out the extra time the class is going since it's over in minutes

```go
ExtraTime("7/25/2019 13:45:00","July 25, 2019 14:46:00")
// => 1 
```

## 4. Display the time elapsed since the start of school 

Implement the `Display` function that takes a time and displays the time passed since the start of the school.

```go
Display("7/25/2019 7:30:00","7/25/2019 13:45:00")
// => "6h15m0s"
```

## 5. Round of the extra time passed by the class in some given seconds

Implement the `RoundOff` function that rounds of the time passed by the class in some given minutes

```go
RoundOff("1h15m30.918273645s","1m0s")
// => 1h15m0s
```
