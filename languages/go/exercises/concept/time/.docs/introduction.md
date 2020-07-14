## Time

In Go, functionality for working with times is provided by the [time package][time_pkg]. The types and methods in this package allow us to manipulate times, get the current time, determine elapsed time, parse times from strings, and more.

To work with time, you will usually just call a method on the type you are working with, but there are also some methods called on the package itself. So if we had a `Time`, say, 6:53:34, stored in a variable called `myTimeVar`, and wanted to extract the minute, we could browse through the `time` package and find the `Minute` method:

```go
    myTimeVar.Minute()
    // 53
```

Parsing strings works a little differently in Go. Many languages use a format string with various codes like YYYY for four-digit year, MM for two-digit month, etc. Go instead uses an exact date, `Mon Jan 2 15:04:05 -0700 MST 2006`, to parse strings. You can rearrange these components or omit them as necessary, and you can spell out month and day names, or use their number values, but in order for Go to understand the parts of the string you are parsing, the corresponding parts of the format string must be from this exact date. Here is a simple example:

```go
    d, _ := time.Parse("Jan 2", "Feb 29")
	// 0000-02-29 00:00:00 +0000 UTC  <- correctly parses date, zeros for omitted parts

    d, _ = time.Parse("Jan 3", "Feb 29")
    // 0001-01-01 00:00:00 +0000 UTC  <- not parsed because Jan 3 is not Go's format string date
```

[time_pkg]: https://golang.org/pkg/time/#pkg-index
