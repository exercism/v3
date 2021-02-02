package iteration

import "testing"

func TestFibonacci(t *testing.T) {
	tests := []struct {
		name string
		n    int
		want int
	}{
		{
			name: "7th number of Fibonacci sequence",
			n:    7,
			want: 13,
		},
		{
			name: "21st number of Fibonacci sequence",
			n:    21,
			want: 10946,
		},
		{
			name: "0th number of Fibonacci sequence",
			n:    0,
			want: 0,
		},
		{
			name: "1st number of Fibonacci sequence",
			n:    1,
			want: 1,
		},
		{
			name: "2nd number of Fibonacci sequence",
			n:    2,
			want: 1,
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			if got := Fibonacci(tt.n); got != tt.want {
				t.Errorf("Fibonacci(%v) = %v, want %v", tt.n, got, tt.want)
			}
		})
	}
}

func TestFibonacciGreater(t *testing.T) {
	tests := []struct {
		name string
		n    int
		want int
	}{
		{
			name: "First number greater than 12",
			n:    12,
			want: 13,
		},
		{
			name: "First number greater than 13",
			n:    13,
			want: 21,
		},
		{
			name: "First number greater than 0",
			n:    0,
			want: 1,
		},
		{
			name: "First number greater than 1",
			n:    1,
			want: 2,
		},
		{
			name: "First number greater than 2",
			n:    2,
			want: 3,
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			if got := FibonacciGreater(tt.n); got != tt.want {
				t.Errorf("FibonacciGreater(%v) = %v, want %v", tt.n, got, tt.want)
			}
		})
	}
}
