A [`Time`][time] in Go is a type describing a moment in time. The date and time information can be accessed, compared, and manipulated through its methods, we have read earlier about some functions called on the `time` package itself. In this exercise we are going to learn about [`Duration`][duration] package.

The type [`Duration`][duration] represents the elapsed time between two instants as an int64 nanosecond count. It can also be found of by subtracting two different [`Time`][time] data types by using the [`Sub`][sub] function.

The [`time.ParseDuration`][parseduration] function parses strings a a duration string. A duration string is a signed sequence of decimal numbers, each with optional fraction and a unit suffix, such as `300ms`, `-1.5h` or `2h45m`.

The `time` package also includes supports for locations/time zones, timers, and other related functionality that will be covered in a later exercise.

[time]: https://golang.org/pkg/time/#Time
[duration]: https://golang.org/pkg/time/#Duration
[now]: https://golang.org/pkg/time/#Now
[sub]: https://golang.org/pkg/time/#example_Time_Sub
[parseduration]: https://golang.org/pkg/time/#ParseDuration
[article]: https://www.pauladamsmith.com/blog/2011/05/go_time.html
