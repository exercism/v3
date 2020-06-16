# Style Guide

## Stub functions

Stub functions should implement a `panic` call, containing the phrase: `please implement the XY function`.

Example:

```go
// Message extracts the message from the provided log line.
func Message(line string) string {
	panic("please implement the Message function")
}
```

## Tests

- Use `table tests` where applicable. The table should be within the test function.
- Use sub-tests (`t.Run`) to execute for the individual tests. This creates the best output for the test runners.
- Don't import any 3rd party test or assert package. The test runners would fail.

Example:

```go
func TestCalculateProductionRatePerHour(t *testing.T) {
	tests := []struct {
		name  string
		speed int
		want  float64
	}{
		{
			name:  "calculate production rate per hour for speed zero",
			speed: 0,
			want:  0.0,
		},
		{
			name:  "calculate production rate per hour for speed one",
			speed: 1,
			want:  221.0,
		},
		{
			name:  "calculate production rate per hour for speed nine",
			speed: 9,
			want:  1531.53,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			got := numbers.CalculateProductionRatePerHour(tt.speed)
			if got != tt.want {
				t.Errorf("CalculateProductionRatePerHour(%d) = %f, want %f", tt.speed, got, tt.want)
			}
		})
	}
}
```

## Test messages

Test messages should take the form

```
FunctionName(%VAL1, %VAL2, %VAL3) = %GOT, want %EXPECTED
```

Examples:

```go
t.Errorf("CalculateProductionRatePerHour(%d) = %f, want %f", tt.speed, got, tt.want)
```

```go
t.Errorf("Message(%s) = %s, want %s", escapeWhiteSpace(tt.line), got, tt.want)
```
