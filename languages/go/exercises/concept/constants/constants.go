package constants

// FixedInterestRate has a value of 5% (5/100)

// GetFixedInterestRate returns the FixedInterestRate constant
func GetFixedInterestRate() float32 {
	panic("Please create the FixedInterestRate constant in the outer scope and then implement the GetFixedInterestRate function")
}

// DaysPerYear has a value of 360

// GetDaysPerYear returns the DaysPerYear constant
func GetDaysPerYear() int {
	panic("Please create the DaysPerYear constant in the outer scope and then implement the GetDaysPerYear function")
}

// Jan-Dec have values of 1-12

// GetMonths returns a slice of the 12 months
func GetMonths() []int {
	panic("Please use a block and the 'iota' identifier in the outer scope to create twelve consecutive constants, one for each month of the year. January shall have a value of 1. Name them 'Jan', 'Feb', 'Mar', etc., and then implement the GetMonths function")
}
