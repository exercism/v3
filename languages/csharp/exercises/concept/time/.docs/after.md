Although this exercise investigates the concept of `time` in practice you rarely deal with times on their own. They are almost always dealt with in conjunction with dates. There is no specific separate _time_ object only [`DateTime`][date-time].

Time-of-day can be expressed with [`TimeSpan`][time-span]. It is not [purpose made][skeet-time-of-day] so the expressiveness of code can get a bit clunky. It does the job.

_time_ has a number of aspects:

- local time vs. universal co-ordinated time (UTC)
- arithmetic on `DateTime`
- Time zones (including daylight saving time)
- Date-time string parsing and formatting.
- Resolution (timer selection etc.)

This exercise covers local vs. UTC, date-time arithmtic, time-zones and date-time parsing.

[Formatting][date-string-formatting] is discussed in the `string-formatting` exercise.

Resolution and timers are [much discussed][so-tiners] on the web. The `DateTime` API has specific routines to handle file timestamps.

It's usually a good idea to store date-times long term in UTC. This ensures consistency if more than one timezone is in play and this approach avoids potential problems with day light saving.

The disadvantage of UTC times is that they need to be converted to local times if time-of-day is a significant factor.

For the most part `DateTime.ToUniversalTime()` and `DateTime.ToLocalTime()` work well as long as processing is based around the timezone of your computer. If multiple time zones are involved or a different one to that of your computer then you will need the `TimeZoneInfo` class to handle conversions. Not the obsolete ~~TimeZone~~ object.

You will recall from the coding exercise that the all-important time zone identifiers differ between Windows and other platforms. This [article][cross-platform-time-zones] is a good introduction to this crooss-platform issue. Note that `TimeZoneInfo.GetSystemTimeZones()` will list your platform's time zones.

This [article][time-overview] is a good overview of time and timezones.

If dates and times are a pervasive and/or critical part of your project then you should investigate [Noda Time][noda-time]

[so-timers]: https://stackoverflow.com/questions/10317088/why-there-are-5-versions-of-timer-classes-in-net
[cross-platforms-time-zones]: https://devblogs.microsoft.com/dotnet/cross-platform-time-zones-with-net-core/
[skeet-time-of-day]: https://stackoverflow.com/a/2037375/96167
[time-overview]: https://docs.microsoft.com/en-us/dotnet/standard/datetime/
[date-time]: https://docs.microsoft.com/en-us/dotnet/api/system.datetime?view=netcore-3.1
[noda-time]: https://nodatime.org/
[date-string-formatting]: https://docs.microsoft.com/en-us/dotnet/standard/base-types/standard-date-and-time-format-strings
[time-span]: https://docs.microsoft.com/en-us/dotnet/api/system.timespan?view=netcore-2.0
