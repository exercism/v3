package numbers_floating_point

import (
	"testing"
)

func TestAnnualBalanceUpdate(t *testing.T) {
	tests := []struct {
		name    string
		balance float64
		want    float64
	}{
		{
			name:    "AnnualBalanceUpdateForEmptyStartBalance",
			balance: 0,
			want:    0,
		},
		{
			name:    "AnnualBalanceUpdateForSmallPositiveStartBalance",
			balance: 0.000001,
			want:    0.000001005,
		},
		{
			name:    "AnnualBalanceUpdateForAveragePositiveStartBalance",
			balance: 1000,
			want:    1016.21,
		},
		{ //test case fails in Golang with float64
			name:    "AnnualBalanceUpdateForLargePositiveStartBalance",
			balance: 1000.0001,
			want:    1016.210101621,
		},
		{
			name:    "AnnualBalanceUpdateForHugePositiveStartBalance",
			balance: 898124017.826243404425,
			want:    920352587.26744292868451875,
		},
		{
			name:    "AnnualBalanceUpdateForSmallNegativeStartBalance",
			balance: -0.123,
			want:    -0.12695199,
		},
		{
			name:    "AnnualBalanceUpdateForLargeNegativeStartBalance",
			balance: -152964.231,
			want:    -157878.97174203,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			got := AnnualBalanceUpdate(tt.balance)
			if got != tt.want {
				t.Errorf(
					"AnnualBalanceUpdate(%f) = %f, want %f",
					tt.balance,
					got,
					tt.want,
				)
			}
		})
	}
}

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

func TestYearsBeforeDesiredBalance(t *testing.T) {
	tests := []struct {
		name          string
		balance       float64
		targetBalance float64
		want          int
	}{
		{
			name:          "YearsBeforeDesiredBalanceForSmallStartBalance",
			balance:       100,
			targetBalance: 125.80,
			want:          47,
		},
		{
			name:          "YearsBeforeDesiredBalanceForAverageStartBalance",
			balance:       1000,
			targetBalance: 1100,
			want:          6,
		},
		{
			name:          "YearsBeforeDesiredBalanceForLargeStartBalance",
			balance:       8080.80,
			targetBalance: 9090.90,
			want:          5,
		},
		{
			name:          "YearsBeforeDesiredBalanceForLargeDifferentBetweenStartAndTargetBalance",
			balance:       2345.67,
			targetBalance: 12345.6789,
			want:          85,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			got := YearsBeforeDesiredBalance(tt.balance, tt.targetBalance)
			if got != tt.want {
				t.Errorf(
					"YearsBeforeDesiredBalance(%f, %f) = %d, want %d",
					tt.balance,
					tt.targetBalance,
					got,
					tt.want,
				)
			}
		})
	}
}
