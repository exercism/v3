package _meta

import "math"

//InterestRate
func InterestRate(balance float64) float64 {
	if balance < 0.0 {
		return -3.213
	}

	if balance < 1000.0 {
		return 0.5
	}

	if balance < 5000.0 {
		return 1.621
	}

	return 2.475

}

func annualYield(balance float64) float64 {
	multiplier := InterestRate(balance) / 100
	return math.Abs(balance) * multiplier
}

//AnnualBalanceUpdate
func AnnualBalanceUpdate(balance float64) float64 {
	return balance + annualYield(balance)
}

//YearsBeforeDesiredBalance
func YearsBeforeDesiredBalance(balance float64, targetBalance float64) int {
	accumulatingBalance := balance
	years := 0

	for accumulatingBalance < targetBalance {
		accumulatingBalance = AnnualBalanceUpdate(accumulatingBalance)
		years++
	}

	return years
}
