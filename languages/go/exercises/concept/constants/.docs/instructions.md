In this exercise you will establish some constants to be used in the operation of a bank. 

You have three tasks:

## 1. Represent the fixed interest rate on a loan

Create the `FixedInterestRate` untyped numeric constant to hold the value of 5% (5/100), then implement the `GetFixedInterestRate` function to return it:

```go
GetFixedInterestRate()
// Output: 0.05
```

## 2. Check if an appointment has already passed

Implement the `HasPassed` function that takes an appointment date and checks if the appointment was somewhere in the past:

```go
HasPassed("July 25, 2019 13:45:00")
// Output: true
```

## 3. Check if appointment is in the afternoon

Implement the `IsAfternoonAppointment` function that takes an appointment date and checks if the appointment is in the afternoon (>= 12:00 and < 18:00):

```go
IsAfternoonAppointment("Thursday, July 25, 2019 13:45:00:00")
// Output: true