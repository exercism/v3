`map` in go is a key-value type. In other programming language, you might know map as `dict` or associative array. The value of an uninitialized map is `nil`, and you can define map as follows

```go
  foo := map[string]int{}
```

or

```go
  foo := make(map[string]int)
```

To retrieve a value map, you can use

```go
baz := foo["bar"]
```

But you have be careful here, if you're trying to retrieve a non existed key, will return the zero value of your value type, to check whether the key exists in your map, you can use

```go
  value, exists := foo["baz"]
  // If the key "baz" doesn't exists,
  // value: 0; exists: false
```
