`map` in go is a key-value type. In other programming language, you might know map as `dict` or associative array. The value of an uninitialized map is `nil`, and you can define map as follows

`make(map[string]int)`

or

```go
  var foo map[string]int
  foo = make(map[string]int)
```

> Here `string` represent the key type of map, and `int` represent the element type of map. You can change them to whatever valid golang type.

To store a value in a map, you can use `=` operator like

`foo["bar"] = 42`

`map` is indexed by its key, therefore, you are required to use different key if you want to store a different data.

To get value from a map, you can use

`foo["bar"]`

Retrieving inexistent key from map will cause a runtime error, and to check if such a key exists you can do

`value, exists := foo["baz"]`

To delete an item from a map, you can use

`delete(foo, "bar")`

If you try to write to a `map` from multiple go routine, that trigger the race detector, [see this link][godoc-race-detector] and [here][goblog-race-detector]. Alternatively, you can use `sync.Map` or `atomic` or `mutex` to work around this issue.

[godoc-race-detector]: https://golang.org/doc/articles/race_detector.html
[goblog-race-detector]: https://golang.org/doc/articles/race_detector.html
