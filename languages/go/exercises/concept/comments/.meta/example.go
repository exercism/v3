// Package weather provides utilities for a
// weather station's program.
package weather

// CurrentCondition describes the current weather condition.
var CurrentCondition string

// InputCurrentCondition updates the exported variable CurrentCondition.
func InputCurrentCondition(condition string) {
	CurrentCondition = condition
}

// Log returns a statement about the current weather condition.
func Log() string {
	return "The current weather condition is: " + CurrentCondition
}
