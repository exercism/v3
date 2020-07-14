package _meta

import (
	"testing"
	"time"
)

func TestSchedule(t *testing.T) {
	tests := map[string]struct {
		in   string
		want time.Time
	}{
		"Schedule 1": {in: "7/13/2020 20:32:00", want: time.Date(2020, time.July, 13, 20, 32, 0, 0, time.UTC)},
		"Schedule 2": {in: "11/28/1984 2:02:02", want: time.Date(1984, time.November, 28, 2, 2, 2, 0, time.UTC)},
		"Schedule 3": {in: "2/29/2112 11:59:59", want: time.Date(2112, time.February, 29, 11, 59, 59, 0, time.UTC)},
	}
	for name, tc := range tests {
		t.Run(name, func(t *testing.T) {
			if got := Schedule(tc.in); !got.Equal(tc.want) {
				t.Errorf("Schedule(%s) = %v, want %v", tc.in, got, tc.want)
			}
		})
	}
}

// func TestHasPassed(t *testing.T) {
// 	t.Skip()
// 	test := struct {
// 		name string
// 		want map[int]int
// 	}{
// 		"HasPassed",
// 		nil,
// 	}
// 	t.Run(test.name, func(t *testing.T) {
// 		if got := HasPassed(); got != nil {
// 			t.Errorf("HasPassed() = %v, want %v", got, test.want)
// 		}
// 	})
// }

// func TestIsAfternoonAppointment(t *testing.T) {
// 	t.Skip()
// 	test := struct {
// 		name string
// 		want []int
// 	}{
// 		"IsAfternoonAppointment",
// 		nil,
// 	}
// 	t.Run(test.name, func(t *testing.T) {
// 		if got := IsAfternoonAppointment(); got != nil {
// 			t.Errorf("IsAfternoonAppointment() = %p, want %p", got, test.want)
// 		}
// 	})
// }

// func TestDescription(t *testing.T) {
// 	t.Skip()
// 	test := struct {
// 		name string
// 		want string
// 	}{
// 		"Description",
// 		"",
// 	}
// 	t.Run(test.name, func(t *testing.T) {
// 		if got := Description(); got != test.want {
// 			t.Errorf("Description() = %s, want %s", got, test.want)
// 		}
// 	})
// }

// func TestAnniversaryDate(t *testing.T) {
// 	t.Skip()
// 	test := struct {
// 		name string
// 		want chan int
// 	}{
// 		"AnniversaryDate",
// 		nil,
// 	}
// 	t.Run(test.name, func(t *testing.T) {
// 		if got := AnniversaryDate(); got != test.want {
// 			t.Errorf("AnniversaryDate() = %v, want %v", got, test.want)
// 		}
// 	})
// }
