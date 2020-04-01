package numbers_floating_point

import (
	"testing"
)

func TestInterestRate(t *testing.T) {
	tests := []struct {
		name    string
		balance float64
		want    float64
	}{
		{
			name:    "MinimalFirstInterestRate",
			balance: 0,
			want:    0.5,
		},
		{
			name:    "TinyFirstInterestRate",
			balance: 0.000001,
			want:    0.5,
		},
		{
			name:    "MaximumFirstInterestRate",
			balance: 999.9999,
			want:    0.5,
		},
		{
			name:    "MinimalSecondInterestRate",
			balance: 1000.0,
			want:    1.621,
		},
		{
			name:    "TinySecondInterestRate",
			balance: 1000.0001,
			want:    1.621,
		},
		{
			name:    "MaximumSecondInterestRate",
			balance: 4999.9990,
			want:    1.621,
		},
		{
			name:    "MinimalThirdInterestRate",
			balance: 5000.0000,
			want:    2.475,
		},
		{
			name:    "TinyThirdInterestRate",
			balance: 5000.0001,
			want:    2.475,
		},
		{
			name:    "LargeThirdInterestRate",
			balance: 5639998.742909,
			want:    2.475,
		},
		{
			name:    "MinimalNegativeInterestRate",
			balance: -0.000001,
			want:    -3.213,
		},
		{
			name:    "SmallNegativeInterestRate",
			balance: -0.123,
			want:    -3.213,
		},
		{
			name:    "RegularNegativeInterestRate",
			balance: -300.0,
			want:    -3.213,
		},
		{
			name:    "LargeNegativeInterestRate",
			balance: -152964.231,
			want:    -3.213,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			got := InterestRate(tt.balance)
			if got != tt.want {
				t.Errorf(
					"InterestRate(%f) = %f, want %f",
					tt.balance,
					got,
					tt.want,
				)
			}
		})
	}
}
