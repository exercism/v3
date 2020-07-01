Although this exercise investigate sthe concept of `time` you rarely deal with times on their own. They are almost always dealt with in conjunction with dates. There is no separate _time_ object only `DateTime`.

Time-of-day can be expressed with `TimeSpan`.  It is not [purpsoe made][skeet-time-of-day] so the expressiveness of code can get a bit clunky.  It does he job.

There are a number of concerns related to _time_:

- local time vs. universal co-ordinated time (UTC)
- Time zones (including daylight saving time)
- Date-time string parsing and formatting.
- Resolution (timer selection etc.)

This exercise covers local vs. UTC, time-zones and date-time parsing.

Formatting is discussed in the `string-formatting` exercise.

Resolution and timers are [much discussed][so-tiners] on the web.

It's usually a good idea to store date-times long term in UTC. This ensures consistency if more than one timezone is in play and this approach avoids potential problems with day light saving.

The disadvantage of UTC times is that they need to be converted to local times if time-of-day is a significant factor.

For the most part `DateTime.ToUniversalTime()` and `DateTime.ToLocalTime()` work well as long as processing is based around the timezone of your computer. If multiple time zones are involved or a different one to that of your computer then you will need the `TimeZoneInfo` class to handle conversions. Not the obsolete ~~TimeZone~~ object.

You will recall from the coding exercise that the all important time zone identifiers differ between Windows and other platforms.

[so-timers]: https://stackoverflow.com/questions/10317088/why-there-are-5-versions-of-timer-classes-in-net
[cross-platforms-time-zones]: https://devblogs.microsoft.com/dotnet/cross-platform-time-zones-with-net-core/
[skeet-time-of-day]: https://stackoverflow.com/a/2037375/96167
