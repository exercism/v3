package _meta

import "testing"

func TestEmptyInterface(t *testing.T) {
	test := struct {
		name string
		want interface{}
	}{
		"return nil",
		nil,
	}
	t.Run(test.name, func(t *testing.T) {
		if got := EmptyInterface(); got != test.want {
			t.Errorf("EmptyInterface returned %s, wanted %s", got, test.want)
		}
	})
}

func TestEmptyMap(t *testing.T) {
	test := struct {
		name string
		want map[int]int
	}{
		"return nil",
		nil,
	}
	t.Run(test.name, func(t *testing.T) {
		if got := EmptyMap(); got != test.want {
			t.Errorf("EmptyMap returned %s, wanted %s", got, test.want)
		}
	})
}

func TestEmptySlice(t *testing.T) {
	test := struct {
		name string
		want []int
	}{
		"return nil",
		nil,
	}
	t.Run(test.name, func(t *testing.T) {
		if got := EmptySlice(); got != test.want {
			t.Errorf("EmptySlice returned %s, wanted %s", got, test.want)
		}
	})
}

func TestEmptyString(t *testing.T) {
	test := struct {
		name string
		want string
	}{
		"return empty string",
		"",
	}
	t.Run(test.name, func(t *testing.T) {
		if got := EmptyString(); got != test.want {
			t.Errorf("EmptyString returned %s, wanted %s", got, test.want)
		}
	})
}

func TestEmptyChannel(t *testing.T) {
	test := struct {
		name string
		want chan int
	}{
		"return nil",
		nil,
	}
	t.Run(test.name, func(t *testing.T) {
		if got := EmptyChannel(); got != test.want {
			t.Errorf("EmptyChannel returned %s, wanted %s", got, test.want)
		}
	})
}

func TestEmptyPointer(t *testing.T) {
	test := struct {
		name string
		want *int
	}{
		"return nil",
		nil,
	}
	t.Run(test.name, func(t *testing.T) {
		if got := EmptyPointer(); got != test.want {
			t.Errorf("EmptyPointer returned %s, wanted %s", got, test.want)
		}
	})
}

func TestEmptyBool(t *testing.T) {
	test := struct {
		name string
		want bool
	}{
		"return false",
		false,
	}
	t.Run(test.name, func(t *testing.T) {
		if got := EmptyBool(); got != test.want {
			t.Errorf("EmptyBool returned %s, wanted %s", got, test.want)
		}
	})
}

func TestEmptyFunc(t *testing.T) {
	test := struct {
		name string
		want func()
	}{
		"return nil",
		nil,
	}
	t.Run(test.name, func(t *testing.T) {
		if got := EmptyFunc(); got != test.want {
			t.Errorf("EmptyFunc returned %s, wanted %s", got, test.want)
		}
	})
}

func TestEmptyInt(t *testing.T) {
	test := struct {
		name string
		want interface{}
	}{
		"return 0",
		0,
	}
	t.Run(test.name, func(t *testing.T) {
		if got := EmptyInt(); got != test.want {
			t.Errorf("EmptyInt returned %s, wanted %s", got, test.want)
		}
	})
}
