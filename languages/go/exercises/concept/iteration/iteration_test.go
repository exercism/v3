package iteration

import "testing"

func TestFibonacci(t *testing.T) {
	type args struct {
		n int
	}
	tests := []struct {
		name string
		args args
		want int
	}{
		{
			name: "7th number of Fibonacci sequence",
			args: args{
				n: 7,
			},
			want: 13,
		},
		{
			name: "21st number of Fibonacci sequence",
			args: args{
				n: 21,
			},
			want: 10946,
		},
		{
			name: "0th number of Fibonacci sequence",
			args: args{
				n: 0,
			},
			want: 0,
		},
		{
			name: "1st number of Fibonacci sequence",
			args: args{
				n: 1,
			},
			want: 1,
		},
		{
			name: "2nd number of Fibonacci sequence",
			args: args{
				n: 2,
			},
			want: 1,
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			if got := Fibonacci(tt.args.n); got != tt.want {
				t.Errorf("Fibonacci(%v) = %v, want %v", tt.args.n, got, tt.want)
			}
		})
	}
}

func TestFibonacciGreater(t *testing.T) {
	type args struct {
		n int
	}
	tests := []struct {
		name string
		args args
		want int
	}{
		{
			name: "First number greater than 12",
			args: args{
				n: 12,
			},
			want: 13,
		},
		{
			name: "First number greater than 13",
			args: args{
				n: 13,
			},
			want: 21,
		},
		{
			name: "First number greater than 0",
			args: args{
				n: 0,
			},
			want: 1,
		},
		{
			name: "First number greater than 1",
			args: args{
				n: 1,
			},
			want: 2,
		},
		{
			name: "First number greater than 2",
			args: args{
				n: 2,
			},
			want: 3,
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			if got := FibonacciGreater(tt.args.n); got != tt.want {
				t.Errorf("FibonacciGreater(%v) = %v, want %v", tt.args.n, got, tt.want)
			}
		})
	}
}
