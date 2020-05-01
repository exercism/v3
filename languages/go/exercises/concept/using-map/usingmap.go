package usingmap

// var priceTable

// TransactionLog represent a single item in a transaction
type TransactionLog struct {
	itemName string
	month    string
	qty      int
}

// CalculateTotal calculates the total sum of all income.
func CalculateTotal(income map[string]int) int {
	panic("Please implement the calculateTotalIncome() function")
}

// CalculateMonthly generates an overview of the income per month.
func CalculateMonthly(transactions []Transaction) (map[string]int, error) {
	panic("Please implement the calculateMonthlyIncome() function")
}
