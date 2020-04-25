package usingmap

import (
	"errors"
)

var priceTable = map[string]int{
	"men_tops":         2,
	"men_underwares":   1,
	"men_jeans":        4,
	"robes":            4,
	"jackets":          7,
	"women_tops":       4,
	"women_underwares": 2,
	"women_jeans":      3,
}

// TransactionLog represent a single item in a transaction
type TransactionLog struct {
	itemName string
	month    string
	qty      int
}

func calculateTotalIncome(monthlyIncome map[string]int) int {
	var total int

	for _, monthlyIncome := range monthlyIncome {
		total += monthlyIncome
	}

	return total
}

func calculateMonthlyIncome(transactionLogs []TransactionLog) (map[string]int, error) {
	monthlyIncome := make(map[string]int)

	for _, transactionLog := range transactionLogs {
		if _, ok := priceTable[transactionLog.itemName]; !ok {
			return nil, errors.New("Item not found")
		}
		monthlyIncome[transactionLog.month] += priceTable[transactionLog.itemName] * int(transactionLog.qty)
	}

	return monthlyIncome, nil
}
