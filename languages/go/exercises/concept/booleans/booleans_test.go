package booleans

import "testing"

func TestCanFastAttack(t *testing.T) {
	tests := []struct {
		name          string
		knightIsAwake bool
		expected      bool
	}{
		{
			name:          "Knight is awake",
			knightIsAwake: true,
			expected:      false,
		},
		{
			name:          "Knight is sleeping",
			knightIsAwake: false,
			expected:      true,
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			if got := CanFastAttack(tt.knightIsAwake); got != tt.expected {
				t.Errorf("CanFastAttack(%v) = %v; want %v", tt.knightIsAwake, tt.knightIsAwake, tt.expected)
			}
		})
	}
}

func TestCanSpy(t *testing.T) {
	tests := []struct {
		desc                      string
		characterStateCombination []bool
		expected                  bool
	}{
		{
			desc:                      "All characters are sleeping",
			characterStateCombination: []bool{false, false, false},
			expected:                  false,
		},
		{
			desc:                      "knight is awake, archer and prisoner are sleeping",
			characterStateCombination: []bool{true, false, false},
			expected:                  true,
		},
		{
			desc:                      "knight and archer are awake, prisoner is sleeping",
			characterStateCombination: []bool{true, true, false},
			expected:                  true,
		},
		{
			desc:                      "knight and prisoner are awake, archer is sleeping",
			characterStateCombination: []bool{true, false, true},
			expected:                  true,
		},
		{
			desc:                      "knight and prisoner are awake, archer is sleeping",
			characterStateCombination: []bool{true, false, true},
			expected:                  true,
		},
		{
			desc:                      "knight and archer are sleeping, prisoner is sleeping",
			characterStateCombination: []bool{false, false, true},
			expected:                  true,
		},
		{
			desc:                      "all characters are awake",
			characterStateCombination: []bool{true, true, true},
			expected:                  true,
		},
	}
	for _, tt := range tests {
		t.Run(tt.desc, func(t *testing.T) {
			if got := CanSpy(tt.characterStateCombination[0], tt.characterStateCombination[1], tt.characterStateCombination[2]); got != tt.expected {
				t.Errorf("CanSpy(%v) = %v; want %v", tt.characterStateCombination, got, tt.expected)
			}
		})
	}
}

func TestCanSignalPrisoner(t *testing.T) {
	tests := []struct {
		desc                      string
		characterStateCombination []bool
		expected                  bool
	}{
		{
			desc:                      "All characters are sleeping",
			characterStateCombination: []bool{false, false},
			expected:                  false,
		},
		{
			desc:                      "archer is sleeping, prisoner is awake",
			characterStateCombination: []bool{false, true},
			expected:                  true,
		},
		{
			desc:                      "archer is awake, prisoner is sleeping",
			characterStateCombination: []bool{true, false},
			expected:                  false,
		},
		{
			desc:                      "All characters are awake",
			characterStateCombination: []bool{true, true},
			expected:                  false,
		},
	}
	for _, tt := range tests {
		t.Run(tt.desc, func(t *testing.T) {
			if got := CanSignalPrisoner(tt.characterStateCombination[0], tt.characterStateCombination[1]); got != tt.expected {
				t.Errorf("CanSignalPrisoner(%v) = %v; want %v", tt.characterStateCombination, got, tt.expected)
			}
		})
	}
}

func TestCanFreePrisoner(t *testing.T) {
	tests := []struct {
		desc                      string
		characterStateCombination []bool
		expected                  bool
	}{
		{
			desc:                      "All characters are sleeping. Dog is not present.",
			characterStateCombination: []bool{false, false, false, false},
			expected:                  false,
		},
		{
			desc:                      "All characters are sleeping. Dog is present.",
			characterStateCombination: []bool{false, false, false, true},
			expected:                  true,
		},
		{
			desc:                      "knight and archer. Prisoner is awake. Dog is not present.",
			characterStateCombination: []bool{false, false, true, false},
			expected:                  true,
		},
		{
			desc:                      "knight and archer. Prisoner is awake. Dog is present.",
			characterStateCombination: []bool{false, false, true, true},
			expected:                  true,
		},
		{
			desc:                      "knight is sleeping. archer is awake. Prisoner is sleeping. Dog is not present.",
			characterStateCombination: []bool{false, true, false, false},
			expected:                  false,
		},
		{
			desc:                      "knight is sleeping. archer is awake. Prisoner is sleeping. Dog is present.",
			characterStateCombination: []bool{false, true, false, true},
			expected:                  false,
		},
		{
			desc:                      "knight is sleeping. archer is awake. Prisoner is awake. Dog is not present.",
			characterStateCombination: []bool{false, true, true, false},
			expected:                  false,
		},
		{
			desc:                      "knight is sleeping. archer is awake. Prisoner is awake. Dog is present.",
			characterStateCombination: []bool{false, true, true, true},
			expected:                  false,
		},
		{
			desc:                      "knight is awake. archer is sleeping. Prisoner is awake. Dog is not present",
			characterStateCombination: []bool{true, false, true, false},
			expected:                  false,
		},
		{
			desc:                      "knight is awake. archer is sleeping. Prisoner is awake. Dog is present",
			characterStateCombination: []bool{true, false, true, true},
			expected:                  true,
		},
		{
			desc:                      "knight is awake. archer is awake. Prisoner is sleeping. Dog is not present",
			characterStateCombination: []bool{true, true, false, false},
			expected:                  false,
		},
		{
			desc:                      "knight is awake. archer is awake. Prisoner is sleeping. Dog is present",
			characterStateCombination: []bool{true, true, false, true},
			expected:                  false,
		},
		{
			desc:                      "knight is awake. archer is awake. Prisoner is awake. Dog is not present",
			characterStateCombination: []bool{true, true, true, false},
			expected:                  false,
		},
		{
			desc:                      "knight is awake. archer is awake. Prisoner is awake. Dog is present",
			characterStateCombination: []bool{true, true, true, true},
			expected:                  false,
		},
	}
	for _, tt := range tests {
		t.Run(tt.desc, func(t *testing.T) {
			if got := CanFreePrisoner(tt.characterStateCombination[0], tt.characterStateCombination[1], tt.characterStateCombination[2], tt.characterStateCombination[3]); got != tt.expected {
				t.Errorf("CanSignalPrisoner(%v) = %v; want %v", tt.characterStateCombination, got, tt.expected)
			}
		})
	}
}
