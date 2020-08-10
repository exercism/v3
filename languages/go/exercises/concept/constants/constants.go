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

// GetJanuary returns the value for Jan, 1
func GetJanuary() int {
	panic("Please use a block and the Go enumerator in the outer scope to create twelve consecutive untyped numeric constants, one for each month of the year. January should have a value of 1. Name them 'Jan', 'Feb', 'Mar', etc., and then implement the GetJanuary function")
}

// GetOctober returns the value for Oct, 10
func GetOctober() int {
	panic("Using the same block as above, implement the GetOctober function")
}

// AccNo type for a string - this is a stub type included to demonstrate how the untyped string constant can be used where this type is required
type AccNo string

// AccountNo has a value of XF348IJ

// GetAccountNumber returns the AccountNo constant
func GetAccountNumber() AccNo {
	panic("Please create the AccountNo untyped string constant in the outer scope and then implement the GetAccountNumber function")
}
