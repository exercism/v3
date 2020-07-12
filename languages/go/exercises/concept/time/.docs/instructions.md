In this exercise you'll be working on an appointment scheduler for a beauty salon in New York that opened on September 15th in 2012.

You have five tasks, which will all involve appointment dates. The dates and times will use one of the following three formats:

- `"7/25/2019 13:45:00"`
- `"July 25, 2019 13:45:00"`
- `"Thursday, July 25, 2019 13:45:00:00"`

## 1. Parse appointment date

Implement the `Schedule` function to parse a textual representation of an appointment date into the corresponding `time.Time` format:

```go
func Schedule("7/25/2019 13:45:00") time.Time {

}
// => 2019-07-25 13:45:00 +0000 UTC
```

## 2. Check if an appointment has already passed

Implement the `HasPassed` function that takes an appointment date and checks if the appointment was somewhere in the past:

```go
func HasPassed("July 25, 2019 13:45:00") bool {

}
// => true
```

## 3. Check if appointment is in the afternoon

Implement the `IsAfternoonAppointment` function that takes an appointment date and checks if the appointment is in the afternoon (>= 12:00 and < 18:00):

```go
Appointment.IsAfternoonAppointment("Thursday, July 25, 2019 13:45:00:00") bool {

}
// => true
```

## 4. See how long until the next appointment

Implement the `HowLongUntilNextAppointment` function that takes an appointment date and returns the duration until that time:

```go
func HowLongUntilNextAppointment("2/15/2029 10:45:00") time.Duration {

}
// => xxxhxxmxxs
```

## 5. Return the anniversary date

Implement the `AnniversaryDate` function that returns this year's anniversary date, which is September 15th:

```go
func AnniversaryDate() string {
    
}
// => new DateTime(2019, 9, 15, 0, 0, 0)
```