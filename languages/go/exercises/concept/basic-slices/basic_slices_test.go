package basic_slices

import (
	"reflect"
	"testing"
)

func TestGetItem(t *testing.T) {
	type args struct {
		slice []uint8
		index int
	}
	tests := []struct {
		name   string
		args   args
		want   uint8
		wantOk bool
	}{
		{
			name: "Retrieve item from slice by index",
			args: args{
				slice: []uint8{5, 2, 255, 32, 182, 7, 0, 33},
				index: 4,
			},
			want:   182,
			wantOk: true,
		},
		{
			name: "Get first item from slice",
			args: args{
				slice: []uint8{5, 2, 255, 32, 182, 7, 0, 33},
				index: 0,
			},
			want:   5,
			wantOk: true,
		},
		{
			name: "Get last item from slice",
			args: args{
				slice: []uint8{5, 2, 255, 32, 182, 7, 0, 33},
				index: 7,
			},
			want:   33,
			wantOk: true,
		},
		{
			name: "Index out of bounds",
			args: args{
				slice: []uint8{5, 2, 255, 32, 182, 7, 0, 33},
				index: 8,
			},
			want:   0,
			wantOk: false,
		},
		{
			name: "Negative index",
			args: args{
				slice: []uint8{5, 2, 255, 32, 182, 7, 0, 33},
				index: -1,
			},
			want:   0,
			wantOk: false,
		},
		{
			name: "Slice is nill",
			args: args{
				slice: nil,
				index: 0,
			},
			want:   0,
			wantOk: false,
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			got, gotOk := GetItem(tt.args.slice, tt.args.index)
			if got != tt.want {
				t.Errorf("GetItem(slice:%v, index:%v) got = %v, want %v", tt.args.slice, tt.args.index, got, tt.want)
			}
			if gotOk != tt.wantOk {
				t.Errorf("GetItem(slice:%v, index:%v) gotOk = %v, want %v", tt.args.slice, tt.args.index, gotOk, tt.wantOk)
			}
		})
	}
}

func TestSetItem(t *testing.T) {
	type args struct {
		slice []uint8
		index int
		value uint8
	}
	tests := []struct {
		name string
		args args
		want []uint8
	}{
		{
			name: "Overwrite an existing item",
			args: args{
				slice: []uint8{5, 2, 255, 32, 182, 7, 0, 33},
				index: 4,
				value: 8,
			},
			want: []uint8{5, 2, 255, 32, 8, 7, 0, 33},
		},
		{
			name: "Overwrite first item",
			args: args{
				slice: []uint8{5, 2, 255, 32, 182, 7, 0, 33},
				index: 0,
				value: 8,
			},
			want: []uint8{8, 2, 255, 32, 182, 7, 0, 33},
		},
		{
			name: "Overwrite last item",
			args: args{
				slice: []uint8{5, 2, 255, 32, 182, 7, 0, 33},
				index: 7,
				value: 8,
			},
			want: []uint8{5, 2, 255, 32, 182, 7, 0, 8},
		},
		{
			name: "Index out of bounds",
			args: args{
				slice: []uint8{5, 2, 255, 32, 182, 7, 0, 33},
				index: 8,
				value: 8,
			},
			want: []uint8{5, 2, 255, 32, 182, 7, 0, 33, 8},
		},
		{
			name: "Negative index",
			args: args{
				slice: []uint8{5, 2, 255, 32, 182, 7, 0, 33},
				index: -1,
				value: 8,
			},
			want: []uint8{5, 2, 255, 32, 182, 7, 0, 33, 8},
		},
		{
			name: "Slice is nill",
			args: args{
				slice: nil,
				index: 7,
				value: 8,
			},
			want: []uint8{8},
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			if got := SetItem(tt.args.slice, tt.args.index, tt.args.value); !reflect.DeepEqual(got, tt.want) {
				t.Errorf("SetItem(slice:%v, index:%v, value:%v) = %v, want %v",
					tt.args.slice, tt.args.index, tt.args.value, got, tt.want)
			}
		})
	}
}

func TestPrefilledSlice(t *testing.T) {
	type args struct {
		value  int
		length int
	}
	tests := []struct {
		name string
		args args
		want []int
	}{
		{
			name: "Create a prefilled slice with value 3",
			args: args{
				value:  3,
				length: 7,
			},
			want: []int{3, 3, 3, 3, 3, 3, 3},
		},
		{
			name: "Create a prefilled slice with value 77",
			args: args{
				value:  77,
				length: 2,
			},
			want: []int{77, 77},
		},
		{
			name: "Length zero",
			args: args{
				value:  3,
				length: 0,
			},
			want: nil,
		},
		{
			name: "Negative length",
			args: args{
				value:  3,
				length: -3,
			},
			want: nil,
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			if got := PrefilledSlice(tt.args.value, tt.args.length); !reflect.DeepEqual(got, tt.want) {
				if tt.want == nil {
					t.Errorf("PrefilledSlice(value:%v, length:%v) = %v, want nil", tt.args.value, tt.args.length, got)
					return
				}
				t.Errorf("PrefilledSlice(value:%v, length:%v) = %v, want %v", tt.args.value, tt.args.length, got, tt.want)
			}
		})
	}
}

func TestNumberRow(t *testing.T) {
	type args struct {
		sumMin int
	}
	tests := []struct {
		name string
		args args
		want []int
	}{
		{
			name: "Create row of numbers",
			args: args{
				sumMin: 33,
			},
			want: []int{1, 2, 3, 4, 5, 6, 7, 8},
		},
		{
			name: "Zero sum",
			args: args{
				sumMin: 0,
			},
			want: nil,
		},
		{
			name: "Sum is 1",
			args: args{
				sumMin: 1,
			},
			want: []int{1},
		},
		{
			name: "Sum is 6",
			args: args{
				sumMin: 6,
			},
			want: []int{1, 2, 3},
		},
		{
			name: "Sum is 7",
			args: args{
				sumMin: 7,
			},
			want: []int{1, 2, 3, 4},
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			if got := NumberRow(tt.args.sumMin); !reflect.DeepEqual(got, tt.want) {
				if tt.want == nil {
					t.Errorf("NumberRow(sumMin:%v) = %v, want nil", tt.args.sumMin, got)
					return
				}
				t.Errorf("NumberRow(sumMin:%v) = %v, want %v", tt.args.sumMin, got, tt.want)
			}
		})
	}
}

func TestRemoveItemPure(t *testing.T) {
	type args struct {
		slice []int
		index int
	}
	tests := []struct {
		name     string
		args     args
		want     []int
		wantOrig []int
	}{
		{
			name: "Remove an item without changing the input slice",
			args: args{
				slice: []int{3, 4, 5, 6},
				index: 1,
			},
			want:     []int{3, 5, 6},
			wantOrig: []int{3, 4, 5, 6},
		},
		{
			name: "Remove the first item",
			args: args{
				slice: []int{3, 4, 5, 6},
				index: 0,
			},
			want:     []int{4, 5, 6},
			wantOrig: []int{3, 4, 5, 6},
		},
		{
			name: "Remove the last item",
			args: args{
				slice: []int{3, 4, 5, 6},
				index: 3,
			},
			want:     []int{3, 4, 5},
			wantOrig: []int{3, 4, 5, 6},
		},
		{
			name: "Remove an item from a nil slice",
			args: args{
				slice: nil,
				index: 1,
			},
			want:     nil,
			wantOrig: nil,
		},
		{
			name: "Remove out of bounds index",
			args: args{
				slice: []int{3, 4, 5, 6},
				index: 7,
			},
			want:     []int{3, 4, 5, 6},
			wantOrig: []int{3, 4, 5, 6},
		},
		{
			name: "Remove negative index",
			args: args{
				slice: []int{3, 4, 5, 6},
				index: -7,
			},
			want:     []int{3, 4, 5, 6},
			wantOrig: []int{3, 4, 5, 6},
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			if got := RemoveItemPure(tt.args.slice, tt.args.index); !reflect.DeepEqual(got, tt.want) {
				t.Errorf("RemoveItemPure() = %v, want %v", got, tt.want)
			}
			if !reflect.DeepEqual(tt.args.slice, tt.wantOrig) {
				t.Errorf("RemoveItemPure changed the input slice to %v, want %v", tt.args.slice, tt.wantOrig)
			}
		})
	}
}

func TestRemoveItem(t *testing.T) {
	type args struct {
		slice []int
		index int
	}
	tests := []struct {
		name string
		args args
		want []int
		orig []int
	}{
		{
			name: "Remove an item without changing the input slice",
			args: args{
				slice: []int{3, 4, 5, 6},
				index: 1,
			},
			want: []int{3, 5, 6},
			orig: []int{3, 4, 5, 6},
		},
		{
			name: "Remove the first item",
			args: args{
				slice: []int{3, 4, 5, 6},
				index: 0,
			},
			want: []int{4, 5, 6},
			orig: []int{3, 4, 5, 6},
		},
		{
			name: "Remove the last item",
			args: args{
				slice: []int{3, 4, 5, 6},
				index: 3,
			},
			want: []int{3, 4, 5},
			orig: []int{3, 4, 5, 6},
		},
		{
			name: "Remove an item from a nil slice",
			args: args{
				slice: nil,
				index: 1,
			},
			want: nil,
		},
		{
			name: "Remove out of bounds index",
			args: args{
				slice: []int{3, 4, 5, 6},
				index: 7,
			},
			want: []int{3, 4, 5, 6},
			orig: []int{3, 4, 5, 6},
		},
		{
			name: "Remove negative index",
			args: args{
				slice: []int{3, 4, 5, 6},
				index: -7,
			},
			want: []int{3, 4, 5, 6},
			orig: []int{3, 4, 5, 6},
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			got := RemoveItem(tt.args.slice, tt.args.index)
			for _, item := range got {
				if count(got, item) != count(tt.want, item) {
					t.Errorf("RemoveItem(slice:%v, index:%v) = %v, want %v", tt.args.slice, tt.args.index, got, tt.want)
				}
			}
			// We definitely expect a change in the input slice if
			// the slice was changed at all if it wasn't the first or last element that was remove.
			if 0 < tt.args.index && tt.args.index < len(tt.orig)-1 {
				if reflect.DeepEqual(tt.args.slice, tt.orig) {
					t.Error("RemoveItemPure did not change the input slice")
				}
			}
		})
	}
}

func count(slice []int, value int) int {
	var sum int
	for _, item := range slice {
		if item == value {
			sum++
		}
	}
	return sum
}
