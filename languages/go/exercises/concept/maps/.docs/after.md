Further Reading:

- [Go Blog - map][goblog-map]
- [Go Spec - map][gospec-map]

Additional Materials:

- [Go Doc - Race Detector][godoc-race-detector]
- [Go Blog - Race Detector][goblog-race-detector]

A `map` in Go is an associative data type that contains a collection of key/value pairs, which other languages might refer to as as dictionary or an associative array, [here's the actual go specification about map][gospec-map].

The value of an uninitialized map is `nil`, and you can define a map as follows:

```go
make(map[string]int)
```

or

```go
  var foo map[string]int
  foo = make(map[string]int)
```

> Here `string` represent the key type of map, and `int` represent the element type of map. You can change them to whatever valid go type.

To store a value in a map, you can use the `=` operator:

```go
  foo["bar"] = 42
```

`map` is indexed by its key, therefore, you are required to use different key if you want to store a different data.

To get value from a map, you can use

```go
  foo["bar"]
```

Retrieving inexistent key from map will cause a runtime error, and to check if such a key exists you can do

```go
  value, exists := foo["baz"]
```

To delete an item from a map, you can use

```go
  delete(foo, "bar")
```

Here are something you need to be aware before using map:

- When you iterate over a map using range loop, go doesn't guarantee the order of the map [goblog-map]

- If you try to write to a `map` from multiple go routine, that trigger the race detector, [see this link][godoc-race-detector] and [here][goblog-race-detector]. Alternatively, you can use `sync.Map` or `atomic` or `mutex` to work around this issue.

[godoc-race-detector]: https://golang.org/doc/articles/race_detector.html
[goblog-race-detector]: https://blog.golang.org/race-detector
[goblog-map]: https://blog.golang.org/maps
[gospec-map]: https://golang.org/ref/spec#Map_types
