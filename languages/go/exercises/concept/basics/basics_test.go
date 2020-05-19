package basics

import (
	"testing"
)

type lasagnaTests struct {
	layers, time, expected int
}

func TestOvenTime(t *testing.T) {
	got := OvenTime()
	if got != 40 {
		t.Errorf("OvenTime(40)  = %d; want 40", got)
	}
}

func TestRemainingOvenTime(t *testing.T) {
	got := RemainingOvenTime(25)
	if got != 15 {
		t.Errorf("RemainingOvenTime(25) = %d; want 15", got)
	}
}
func TestPreparationTime(t *testing.T) {
	var tests = []lasagnaTests{
		{ //one layer
			layers:   1,
			expected: 2,
		},
		{ //multiple layers
			layers:   4,
			expected: 8,
		},
	}

	for _, tc := range tests {
		got := PreparationTime(tc.layers)
		if got != tc.expected {
			t.Errorf("PreparationTime(%d) = %d; want %d", tc.layers, got, tc.expected)
		}
	}
}

func TestTotalTime(t *testing.T) {
	var tests = []lasagnaTests{
		{ //one layer
			layers:   1,
			time:     30,
			expected: 32,
		},
		{ //multiple layers
			layers:   4,
			time:     8,
			expected: 16,
		},
	}
	for _, tc := range tests {
		got := TotalTime(tc.layers, tc.time)
		if got != tc.expected {
			t.Errorf("PreparationTime(%d, %d) = %d; want %d", tc.layers, tc.time, got, tc.expected)
		}
	}
}
