package _meta

// Fibonacci returns the nth number of the fibonacci sequence where the 0 is considered the 0th number, 1 the first number.
// The fibonacci sequence starts with the numbers 0, 1.
// The next number is always the sum of the 2 previous ones.
// First numbers are: 0, 1, 1, 2, 3, 5, 8, 13, 21
func Fibonacci(n int) int {
	if n < 2 {
		return n
	}
	x1, x2 := 1, 1
	for i := 2; i < n; i++ {
		x1, x2 = x2, x1+x2
	}
	return x2
}

// FibonacciGreater finds the first fibonacci number greater than the given `n`.
func FibonacciGreater(n int) int {
	x1, x2 := 1, 1
	for x2 <= n {
		x1, x2 = x2, x1+x2
	}
	return x2
}
