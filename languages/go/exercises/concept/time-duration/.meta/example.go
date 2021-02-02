package _meta

import (
	"fmt"
	"time"
)

// StartTime parses a textual representation of a time duration of the class into the corresponding time.Duration format and return the duration:
func StartTime(date string) time.Duration {
	complex, _ := time.ParseDuration(date)
	return complex
}

// IsOver function that takes the start time and a given time and returns out whether the current class is over or not 
func IsOver(date string,date1 string) bool {
	t1 := time.Date(date)
  t2 := time.Date(date1)
  t=t2.Sub(t1)
	return t.minutes() >= 60
}

// ExtraTime function that takes a time and finds out the extra time the class is going since it's over in minutes
func ExtraTime(date string,date1 string) float64 {
	t1 := time.Date(date)
  t2 := time.Date(date1)
  t=t2.Sub(t1)
	return (t.minutes.()-60)
}

// Display function that takes a time and displays the time passed since the start of the school.
func Display(date string,date1 string) string {
	t1 := time.Date(date)
  t2 := time.Date(date1)
  t=t2.Sub(t1)
	return fmt.Sprintf(t.String())
}

// RoundOff function that rounds of the time passed by the class in some given minutes
func RoundOff() time.Duration {
	d, err := time.ParseDuration("1h15m30.918273645s")
  return d.Truncate(t)
}
