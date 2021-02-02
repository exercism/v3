# Introduction

### Iteration

At first glance Go only has a `for` loop. When looking closer though it becomes obvious that the for loop in Go is quite versatile.
1. It can be used with a counter and a known number of iterations: `for i := 0; i < n; i++ {...}`.
2. It can be used only with a condition to stop on : `for i < n {...}`. In many languages this is known as a `while` loop.
3. It can be used to create a loop without any abort condition -- an endless loop: `for {...}`.
This is either used with `break`/`return` to stop from inside the loop or to actually have a loop run endlessly.
4. It can be used to iterate over a `slice` or `map` with the `range` keyword: `for i, item := range someList {...}`. 
`Slice` and `map` have not beeen introduced yet which is why this will be covered later.
5. Last but not least a `for` loop can be used to iterate over a `channel`: `for item := range chItems {...}`.
This will also be covered in a later exercise.

This exercise will cover the first 3 cases. The `range` loop will be covered in later exercises.

### Conditionals

This exercise also introduces _conditionals_. Here is a little intro:
[Go by Example: If/Else](https://gobyexample.com/if-else)
