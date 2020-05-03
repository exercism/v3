package maps

// units store the Dozen Store unit measurement
var units = map[string]int{
	"quarter_of_a_dozen": 3,
	"half_of_a_dozen":    6,
	"dozen":              12,
	"small_gross":        120,
	"gross":              144,
	"great_gross":        1728,
}

// bill to store the item that customer wants to buy
var bill = map[string]int{}

// AddItem add item to customer bill
func AddItem(item string, unit string) bool {
	if _, ok := units[unit]; !ok {
		return false
	}

	bill[item] += units[unit]

	return true
}

// RemoveItem remove item from customer bill
func RemoveItem(item string, unit string) bool {
	if _, ok := bill[item]; !ok {
		return false
	}

	if _, ok := units[unit]; !ok {
		return false
	}

	newUnit := bill[item] - units[unit]
	if newUnit < 0 {
		return false
	} else if newUnit == 0 {
		delete(bill, item)
	} else {
		bill[item] -= units[unit]
	}

	return true
}

// Checkout finish the transaction, reset the bill to zero
func Checkout() {
	bill = make(map[string]int)
}

// GetItem return the quantity of item that the customer has in his/her bill
func GetItem(item string) (int, bool) {
	if _, ok := bill[item]; !ok {
		return 0, false
	}

	return bill[item], true
}
