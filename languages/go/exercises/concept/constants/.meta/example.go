package _meta

// FixedInterestRate has a value of 5%
const FixedInterestRate = 0.05

// GetFixedInterestRate returns the FixedInterestRate constant and gives it a type of float32
func GetFixedInterestRate() float32 {
	return FixedInterestRate
}

// DaysPerYear has a value of 360
const DaysPerYear int = 360

// GetDaysPerYear returns the DaysPerYear constant
func GetDaysPerYear() int {
	return DaysPerYear
}

// Jan-Dec have values of 1-12
const (
	Jan = iota + 1
	Feb
	Mar
	Apr
	May
	Jun
	Jul
	Aug
	Sep
	Oct
	Nov
	Dec
)

// GetMonths returns a slice of the months
func GetMonths() []int {
	return []int{Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec}
}
