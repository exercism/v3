## Constants

In Go, a constant is a simple, unchanging value assigned to a name with the `const` keyword:

```go
    const myName = "Exercism"
```

Go has a keyword for creating enumerated constants called `iota`. Constants in a block are implicitly repeated:

```go
    const (
        a = 9
        b
        c
        d = iota
        e
        f
        g
    )
    fmt.Print(a, b, c, d, e, f, g)
    // Output: 9 9 9 3 4 5 6
```
