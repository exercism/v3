package basic_slices

// GetItem retrieves an item from a slice at given position. The second return value indicates whether
// a the given index existed in the slice or not.
func GetItem(slice []uint8, index int) (uint8, bool) {
	panic("not implemented")
}

// SetItem writes an item to a slice at given position overwriting an existing value.
// If the index is out of range it is be appended.
func SetItem(slice []uint8, index int, value uint8) []uint8 {
	panic("not implemented")
}

// PrefilledSlice creates a slice of given length and prefills it with the given value.
func PrefilledSlice(value, length int) []int {
	panic("not implemented")
}

// NumberRow fills a slice with the numbers 1, 2, 3, etc. It returns as many numbers as are necessary for the sum
// of the slice to be equal or larger than `sumMin`.
func NumberRow(sumMin int) []int {
	panic("not implemented")
}

// RemoveItemPure removes an item without changing the input values or other side effects.
// In functional programming this is called a `pure` function.
func RemoveItemPure(slice []int, index int) []int {
	panic("not implemented")
}

// RemoveItem removes an item from a slice by modifying the existing slice (without allocating new memory).
// The order of items in the slice might not be the same as before.
func RemoveItem(slice []int, index int) []int {
	panic("not implemented")
}
