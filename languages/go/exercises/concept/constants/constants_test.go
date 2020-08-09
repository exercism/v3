package constants

import (
	"testing"
)

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

func TestGetJanuary(t *testing.T) {
	tests := map[string]struct {
		want int
	}{
		"GetJanuary 1": {want: 1},
	}
	for name, tc := range tests {
		t.Run(name, func(t *testing.T) {
			if got := GetJanuary(); got != tc.want {
				t.Errorf("GetJanuary() = %v, want %v", got, tc.want)
			}
		})
	}
}

func TestGetOctober(t *testing.T) {
	tests := map[string]struct {
		want int
	}{
		"GetOctober 1": {want: 10},
	}
	for name, tc := range tests {
		t.Run(name, func(t *testing.T) {
			if got := GetOctober(); got != tc.want {
				t.Errorf("GetOctober() = %v, want %v", got, tc.want)
			}
		})
	}
}

func TestGetAccountNumber(t *testing.T) {
	tests := map[string]struct {
		want AccNo
	}{
		"GetAccountNumber 1": {want: "XF348IJ"},
	}
	for name, tc := range tests {
		t.Run(name, func(t *testing.T) {
			if got := GetAccountNumber(); got != tc.want {
				t.Errorf("GetAccountNumber() = %v, want %v", got, tc.want)
			}
		})
	}
}
