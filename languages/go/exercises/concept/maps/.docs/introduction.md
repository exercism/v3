`map` in go is a key-value type. In other programming language, you might know map as `dict` or associative array. The value of an uninitialized map is `nil`, and you can define map as follows

```go
  make(map[string]int)
```

or

```go
  var foo map[string]int
  foo = make(map[string]int)
```
