package _meta

import (
	"reflect"
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

func TestGetMonths(t *testing.T) {
	tests := map[string]struct {
		want []int
	}{
		"GetMonths 1": {want: []int{1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12}},
	}
	for name, tc := range tests {
		t.Run(name, func(t *testing.T) {
			if got := GetMonths(); !reflect.DeepEqual(got, tc.want) {
				t.Errorf("GetMonths() = %v, want %v", got, tc.want)
			}
		})
	}
}
