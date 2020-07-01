Although this exercise investigate sthe concept of `time` you rarely deal with times on their own. They are almost always dealt with in conjunction with dates. There is no separate _time_ object only `DateTime`.

There is no built-in way to express the idea of time-of-day, useful, for example, if you had a recurring appointment at the same time every day. Of course, it is a trivial matter to fashion your own but you have the burden of making your class inter-operate with `DateTime`, `TimeSpan` etc..

There are a number of concerns related to _time_:

- Time zones (including daylight saving time)
- Date-time string parsing and formatting.
- Resolution (timer selection etc.)

This exercise covers time-zones and date-time parsing.

Formatting is discussed in the `string-formatting` exercise.

Resolution and timers are [much discussed][so-tiners] on the web.

[so-timers]: https://stackoverflow.com/questions/10317088/why-there-are-5-versions-of-timer-classes-in-net
