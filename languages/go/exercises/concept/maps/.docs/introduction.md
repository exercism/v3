`map` in go is a key-value type. In other programming language, you might know map as `dict` or associative array. The value of an uninitialized map is `nil`, and you can define map as follows

```go
  make(map[string]int)
```

or

```go
  var foo map[string]int
  foo = make(map[string]int)
```

To retrieve a value map, you can use

```go
baz := foo["bar"]
```

But you have be careful here, if you're trying to retrieve a non existed key, it will cause a [run-time panic][gospec-run-time-panic], to avoid run-time panic, you can use

```go
  value, exists := foo["baz"]
```

[gospec-run-time-panic]: https://golang.org/ref/spec#Run_time_panics
