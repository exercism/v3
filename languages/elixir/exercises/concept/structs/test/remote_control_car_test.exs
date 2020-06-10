defmodule RemoteControlCarTest do
  use ExUnit.Case

  test "new" do
    car = RemoteControlCar.new()
    assert car.battery_percentage == 100
    assert car.distance_driven_in_meters == 0
    assert Map.equal?(car, %RemoteControlCar{})
  end

  test "drive with battery" do
    car = RemoteControlCar.new()
    assert car
      |> RemoteControlCar.drive()
      |> Map.equal?(%RemoteControlCar{battery_percentage: 99, distance_driven_in_meters: 20})
  end

  test "drive with dead battery" do
    car = RemoteControlCar.new()
    car = %{car | battery_percentage: 0}
    assert car
      |> RemoteControlCar.drive()
      |> Map.equal?(%RemoteControlCar{battery_percentage: 0, distance_driven_in_meters: 0})
  end

  test "display distance of new" do
    car = RemoteControlCar.new()
    assert RemoteControlCar.display_distance(car) == "0 meters"
  end

  test "display distance of driven" do
    car = RemoteControlCar.new()
    car = %{car | distance_driven_in_meters: 20}
    assert RemoteControlCar.display_distance(car) == "20 meters"
  end

  test "display battery of new" do
    car = RemoteControlCar.new()
    assert RemoteControlCar.display_battery(car) == "Battery at 100%"
  end

  test "display battery of driven" do
    car = RemoteControlCar.new()
    car = %{car | battery_percentage: 0}
    assert RemoteControlCar.display_battery(car) == "Battery empty"
  end
end
