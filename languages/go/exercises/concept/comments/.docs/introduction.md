In the previous exercise, we saw that there are two ways to write comments in Go: single line comments that are preceded by `//`, and multiline comment blocks that are wrapped with `/*` and `*/`.

## Documentation comments

In Go, comments play an important role in documenting code. They are used by the tool [godoc][godoc], which extracts these comments to create documentation about Go packages. A documentation comment should be a complete sentence that starts with the name of the thing being described and ends with a period. 

Comments should precede packages as well as exported identifiers, e.g. exported functions, methods, package variables, constants, and structs, which you will learn more about in the next exercises.

A package variable can look like this:

```go
// CurrentTemperature gives the current 
// temperature in degrees Celsius.
var CurrentTemperature int
```

## Package comments

Package comments should be written directly before a package and begin with `Pacakge X ...` like this: 

```go
// Package kelvin provides tools to convert
// temperatures to and from Kelvin.
package kelvin
```

## Function comments

A function comment can look like this:
```go
// CelsiusFreezingTemp returns the temp
// at which water freezes in degrees Celsius.
func CelsiusFreezingTemp() int {
	return 0
} 
```

## Golint

[Golint][golint] is a great tool to check for missing comments and other common stylistic issues, which you can read more about at [Effective Go][effective go].

You can install `golint` on your machine with the following command:

```
go get -u golang.org/x/lint/golint
````

It's a good idea to configure your editor to run `golint` for you, otherwise you can invoke it like this:

```
golint weather.go
```

To use this command globally, make sure that `golint` is in your $PATH.


[godoc]: https://blog.golang.org/godoc
[golint]: https://github.com/golang/lint
[effective go]: https://golang.org/doc/effective_go.html