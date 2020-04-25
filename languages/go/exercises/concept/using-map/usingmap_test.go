package usingmap

import (
	"reflect"
	"testing"
)

func TestCalculateIncome(t *testing.T) {
	tests := []struct {
		transactionLogs []TransactionLog
		totalIncome     int
		monthlyIncome   map[string]int
	}{
		{
			transactionLogs: []TransactionLog{
				{"men_tops", "June", 1},
			},
			totalIncome: 2,
			monthlyIncome: map[string]int{
				"June": 2,
			},
		},
		{
			transactionLogs: []TransactionLog{
				{"men_tops", "June", 1},
				{"jackets", "July", 1},
				{"men_tops", "May", 1},
				{"women_tops", "January", 1},
				{"women_jeans", "January", 1},
			},
			totalIncome: 18,
			monthlyIncome: map[string]int{
				"January": 7,
				"July":    7,
				"June":    2,
				"May":     2,
			},
		},
		{
			transactionLogs: []TransactionLog{
				{"men_tops", "June", 6},
				{"jackets", "July", 3},
				{"jackets", "December", 1},
				{"men_tops", "May", 1},
				{"women_tops", "August", 5},
				{"jackets", "December", 1},
				{"women_jeans", "September", 1},
				{"robes", "March", 5},
				{"jackets", "December", 10},
				{"men_tops", "January", 15},
				{"men_jeans", "October", 20},
			},
			totalIncome: 272,
			monthlyIncome: map[string]int{
				"August":    20,
				"December":  84,
				"January":   30,
				"July":      21,
				"June":      12,
				"March":     20,
				"May":       2,
				"October":   80,
				"September": 3,
			},
		},
	}

	for _, tt := range tests {
		monthlyIncome, err := calculateMonthlyIncome(tt.transactionLogs)
		if err != nil {
			t.Errorf("Want no error, found: %w", err)
			return
		}

		if !reflect.DeepEqual(monthlyIncome, tt.monthlyIncome) {
			t.Errorf("Want monthly income \n  %+v, found \n +%v\n", tt.monthlyIncome, monthlyIncome)
			return
		}

		totalIncome := calculateTotalIncome(monthlyIncome)
		if totalIncome != tt.totalIncome {
			t.Errorf("Want total income %d, found %d\n", tt.totalIncome, totalIncome)
			return
		}

	}
}

func TestShouldReturnErrorForNonexistentKey(t *testing.T) {

	tests := []struct {
		transactionLogs []TransactionLog
		shouldError     bool
	}{
		{[]TransactionLog{{"men_tops", "January", 1}}, false},
		{[]TransactionLog{{"hat", "March", 1}}, true},
	}

	for _, tt := range tests {
		_, err := calculateMonthlyIncome(tt.transactionLogs)
		if tt.shouldError && err == nil {
			t.Errorf("Want an error, found: %w", err)
			return
		}

		if !tt.shouldError && err != nil {
			t.Errorf("Want no error, found: %w", err)
			return
		}

	}
}
