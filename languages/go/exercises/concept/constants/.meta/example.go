package _meta

const FixedInterestRate = 0.05

func GetFixedInterestRate() float32 {
	return FixedInterestRate
}

const DaysPerYear int = 360

func GetDaysPerYear() int {
	return DaysPerYear
}

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

func GetMonths() []int {
	return []int{Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec}
}
