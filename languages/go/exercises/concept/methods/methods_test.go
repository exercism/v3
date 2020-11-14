package methods

import "testing"

func TestCreateCar(t *testing.T){
		tests := []struct{
		name string
		car Car
		expected Car
	}{
		{
			name: "Create a new car.",
			car: Car{
				speed: 5,
				batteryDrain: 2,
				battery: 100, 
			},
			expected: Car{
				speed: 5,
				batteryDrain: 2,
				battery: 100,
			},

		},
	}

	for _,tt:=range tests{
		t.Run(tt.name,func(t *testing.T){
			got := CreateCar(tt.car.speed, tt.car.batteryDrain)

			if got!= &tt.expected{
				t.Errorf("CreateCar(%v,%v) = %v; expected %v", tt.car.speed, tt.car.batteryDrain, got, tt.expected)
			}
		})
	}
}

func TestCreateTrack(t *testing.T){
	tests :=[]struct{
		name string
		track Track
		expected Track
	}{
		{
			name: "Create a new track.",
			track: Track{
				distance: 800,
			},
			expected: Track{
				distance: 800,
			},
		},
		{
			name: "Create a new track.",
			track: Track{
				distance: 360,
			},
			expected: Track{
				distance: 360,
			},
		},
	}


	for _,tt :=range tests{
		t.Run(tt.name, func(t *testing.T){
			got := CreateTrack(tt.track.distance)

			if got!=tt.expected{
				t.Errorf("CreateTrack(%v) = %v; expected %v", tt.track.distance, tt.track, tt.expected)
			}
		})
	}
}

func TestDrive(t *testing.T){
	tests := []struct{
		name string
		car Car
		expected Car
	}{
		{
			name: "Drive the car once.",
			car: Car{
				speed: 5,
				batteryDrain: 2,
				battery: 100,
				distance:0,
			},
			expected: Car{
				speed: 5,
				batteryDrain: 2,
				battery: 100,
				distance: 5,
			},

		},
	}

	for _,tt:=range tests{
		t.Run(tt.name, func(t *testing.T){
			car :=tt.car
			println(">>", car.Drive())
			// got:= car.Drive

			// if got!=tt.expected {
			// 	t.Errorf("method Drive() = %v; expected %v", got, tt.expected)
			// }

		})
	}
}

func TestCanFinish(t *testing.T){
	tests := []struct{
		name string
		car Car
		track Track
		expected bool
	}{
		{
			name:"Car has 100% battery. Car can finish the race",
			car: Car{
				speed: 5,
				batteryDrain: 2,
				battery: 100,
				distance:0,
			},
			track: Track{
				distance: 100,
			},
			expected: true,
		},
	}

	for _,tt:=range tests{
		t.Run(tt.name, func(t *testing.T){
			got := tt.car.CanFinish(tt.track)

			if got!=tt.expected{
				t.Errorf("method CanFinish(%v) = %v. Expected value %v", tt.track, got, tt.expected)
			}
		})
	}
}


func TestDisplayDistance(t *testing.T){
	// TBA
}

func TestDisplayBatteryPercentage(t *testing.T){
	// TBA
}