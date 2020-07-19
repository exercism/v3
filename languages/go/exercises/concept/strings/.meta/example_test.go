package _meta

import (
	"fmt"
	"testing"
)

func TestAssignTable(t *testing.T) {
	type args struct {
		name      string
		table     int
		neighbour string
		direction string
		distance  float64
	}
	tests := []struct {
		name string
		args args
		want string
	}{
		// TODO: Add test cases.
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			if got := AssignTable(tt.args.name, tt.args.table, tt.args.neighbour, tt.args.direction, tt.args.distance); got != tt.want {
				t.Errorf("AssignTable() = %v, want %v", got, tt.want)
			}
		})
	}

	fmt.Println(AssignTable("Christiane", 27, "Frank", "on the left", 23.7834298))
	t.Fail()
}
