package _meta

import "testing"

func TestGetFixedInterestRate(t *testing.T) {
	tests := map[string]struct {
		want float32
	}{
		"GetFixedInterestRate 1": {want: 0.05},
	}
	for name, tc := range tests {
		t.Run(name, func(t *testing.T) {
			if got := GetFixedInterestRate(); got != tc.want {
				t.Errorf("GetFixedInterestRate() = %v, want %v", got, tc.want)
			}
		})
	}
}

func TestGetDaysPerYear(t *testing.T) {
	tests := map[string]struct {
		want int
	}{
		"GetDaysPerYear 1": {want: 360},
	}
	for name, tc := range tests {
		t.Run(name, func(t *testing.T) {
			if got := GetDaysPerYear(); got != tc.want {
				t.Errorf("GetDaysPerYear() = %v, want %v", got, tc.want)
			}
		})
	}
}
