In this exercise you're going to write an iterator that iterates through the first `n` elements of the [Fibonacci sequence](https://en.wikipedia.org/wiki/Fibonacci_number).

## Fibonacci sequence

The Fibonacci sequence is defined recursively.
Each element of the sequence is the sum of the two previous elements.
The first two elements of the sequence are set to 1.
In maths notation:

$$

a_1 = a_2 = 1\\
a_n = a_{n-1} + a_{n-2}

$$

For example, the third element would be $a_3 = a_1 + a_2 = 1 + 1 = 2$, the fourth would be $a_4 = a_2 + a_3 = 1 + 2 = 3$, and so on.

The first elements of the sequence are $1,~1,~2,~3,~5,~8,~13,~21,~34,~55,~89,~\dots$

!!! note
    Some sources define the first elements as $a_0 = 0$ and $a_1 = 1$.
    However in the context of this exercise, we define the sequence without a 0-th element.

## 1. Define a `Fib` type with a constructor that takes `n` as argument

```julia
julia> Fib(10)
Fib(10)
```

## 2. Implement `iterate` methods

```julia
julia> for a in Fib(10)
           println(a)
       end
1
1
2
3
5
8
13
21
34
55
```

You can ignore integer overflow in your implementation.
The tests will only test for `n` small enough to not cause overflow problems.

## 3. Define the optional methods that make `collect` work

```julia
julia> collect(Fib(10))
10-element Array{Any,1}:
  1
  1
  2
  3
  5
  8
 13
 21
 34
 55
```

<!-- TODO: This may be out of scope -->
## 4. Define the optional methods that are necessary for Julia to infer the type of the elements of the iterator

```julia
julia> collect(Fib(10))
10-element Array{Int64,1}:
  1
  1
  2
  3
  5
  8
 13
 21
 34
 55
```
