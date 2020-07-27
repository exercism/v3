package time

import (
	"testing"
	"time"
)

func TestStartTime(t *testing.T) {
	tests := map[string]struct {
		in   string
		want time.Duration
	}{
		"StartTime 1": {in: "10h", time.ParseDuration("10h")},
		"StartTime 2": {in: "1h10m10s", want: time.ParseDuration("1h10m10s")},
		"StartTime 3": {in: "2h", want: time.ParseDuration("2h")},
	}
	for name, tc := range tests {
		t.Run(name, func(t *testing.T) {
			if got := StartTime(tc.in); got != tc.want {
				t.Errorf("StartTime(%s) = %v, want %v", tc.in, got, tc.want)
			}
		})
	}
}

func TestIsOver(t *testing.T) {
	tests := map[string]struct {
		in   string
		in   string
		want bool
	}{
		"IsOver 1": {in: "7/25/2019 13:45:00",in: "July 25, 2019 14:46:00", want: true},
		"IsOver 2": {in: "7/25/2019 8:45:00",in: "July 25, 2019 9:46:00", want: true},
		"IsOver 3": {in: "7/25/2019 12:45:00",in: "July 25, 2019 13:36:00", want: false},
	}
	for name, tc := range tests {
		t.Run(name, func(t *testing.T) {
			if got := IsOver(tc.in); got != tc.want {
				t.Errorf("IsOver(%s) = %v, want %v", tc.in, got, tc.want)
			}
		})
	}
}

func TestExtraTime(t *testing.T) {
	tests := map[string]struct {
		in   string
		in   string
		want float64
	}{
		"ExtraTime 1": {in: "7/25/2019 13:45:00",in: "July 25, 2019 14:46:00", want: 1},
		"ExtraTime 2": {in: "7/25/2019 12:45:00",in: "July 25, 2019 13:46:00", want: 1},
		"ExtraTime 3": {in: "7/25/2019 8:45:00",in: "July 25, 2019 9:46:00", want: 1},
	}
	for name, tc := range tests {
		t.Run(name, func(t *testing.T) {
			if got := IsExtraTime(tc.in); got != tc.want {
				t.Errorf("ExtraTime(%s) = %v, want %v", tc.in, got, tc.want)
			}
		})
	}
}

func TestDisplay(t *testing.T) {
	tests := map[string]struct {
		in string
		in string
		want string
	}{
		"Display 1": {in: "7/25/2019 7:30:00",in: "7/25/2019 13:45:00", want: "6h15m0s"},
		"Display 2": {in: "7/25/2019 7:30:00",in: "7/25/2019 13:45:00", want: "6h15m0s"},
		"Display 3": {in: "7/25/2019 7:30:00",in: "7/25/2019 13:45:00", want: "6h15m0s"},
	}
	for name, tc := range tests {
		t.Run(name, func(t *testing.T) {
			if got := Display(tc.in); got != tc.want {
				t.Errorf("Display(%s) = %v, want %v", tc.in, got, tc.want)
			}
		})
	}
}
func TestRoundOff(t *testing.T) {
	tests := map[string]struct {
		in  string
		in  string
		want time.Duration
	}{
		"RoundOff 1": {in: "1h15m30.918273645s",in: "1m0s" ,want: "1h15m0s" },
	}
	for name, tc := range tests {
		t.Run(name, func(t *testing.T) {
			if got := RoundOff();  got != tc.want {
				t.Errorf("RoundOff(%s) = %v, want %v", tc.in, got, tc.want)
			}
		})
	}
}
