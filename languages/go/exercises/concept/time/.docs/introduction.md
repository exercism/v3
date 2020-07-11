## Time

In Go, functionality for working with times is provided by the [time package][time_pkg]. The Time and Duration types and their methods allow us to manipulate times, get the current time, determine elapsed time, parse times from strings, and more.

Working with times is simple: typically you would just call a method on the type you are working with. So if we had a Time, say, 6:53:34, and want to extract the minute, we can browse through the time package and find the time.Minute() method:

```go
    myTime.Minute()
    // 53
```

[time_pkg]: https://golang.org/pkg/time/#pkg-index
