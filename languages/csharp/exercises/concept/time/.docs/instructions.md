In this exercise you are back in the world of salons (first introduced in the `datetimes`) exercise. As with a number of your projects your client has had much success and opened outlets in London and Paris in addition the New York base.

## 1. Provide local time equivalents of utc appointments for the administrators

Implement the static `Appointment.ShowLocalTime()` method that takes a utc time and returns it as a local time

```csharp
// For a student in NY
Appointment.ShowLocalTime(new DateTime(2030, 7, 25, 13, 45, 0));
// => {2030, 7, 25, 8, 45, 0}
```

## 2. Schedule appointments in New York, London and Paris

Implement the static `Appointment.Shedule()` overload which takes a location and time string and returns the utc time of the appointment.

Salons are responsible for taking their own bookings. The time input is local to the location of the salon. For instance, if someone enters a time of 13:45 for the New York salon for an appointment, they would expect the client to turn up just after lunch. Similarly, if someone entered a time of 13:45 for the London salon they would also expect the client arrive just after lunch.

It will help you to know the time zone id for New York, London and Paris.

On Mac and Linux these are:

New York - America/New_York
London - Europe/London
Paris - Europe/Paris

On Windows they are:

New York - Eastern Standard Time
London - GMT Standard Time
Paris - W. Europe Standard Time

The date-time strings input are guaranteed to be valid.

```csharp
Appointment.Schedule("7/25/2030 13:45:00", Location.Paris);
// => {2030, 7, 25, 12, 45, 0}
```

## 3. Provide alerts to clients at intervals before the appointment

Implement the static `Appointment.GetAlertTime()` to provide alerts at 1 day, 1 hour 45 minutes and 30 minutes before the appointment.

```csharp
Appointment.GetAlertTime(new DateTime(2030, 7, 25, 14, 45, 0), AlertLeel.Early);
// => {2030, 7, 24, 14, 45, 0}
```

## 4. If daylight savings has recently changed we send a message to clients reminding them.

Implement the static `Appointment.HasDaylightSavingChanged()` to return `true` if the daylight savings has become active or inactive in the last 7 days.

```csharp
Appointment.HasDaylightSavingChanged(new DateTime(2020, 3, 30, 14, 45, 0), Location.London);
// => true
```

## 5. Use the local date time format to enter appointments

The Brits and the French are looking for a bit of respect so you have been tasked with creating an experimental routine to allow values to be entered in the default format for the location of the salon.

Implement the `Appointment.Normalize()` method that takes a well-formed date-time string in an format appropriate to the location and converts it into a `DateTime` object. No attempt is made to convert the date-time to UTC.

If a bad format is entered then a `DateTime` with a value of 1/1/1 should be returned.

```csharp
Appointment.NormalizeDateTime(new DateTime(2020, 3, 30, 14, 45, 0), Location.London);
// => true
```
