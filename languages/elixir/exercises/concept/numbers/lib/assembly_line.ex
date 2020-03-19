defmodule AssemblyLine do

  @spec availability(integer()) :: float()
  def availability(minutes_worked) do
    raise "Not Yet Implemented"
  end

  @spec performance(integer()) :: float()
  def performance(cars_produced) do
    raise "Not Yet Implemented"
  end

  @spec yield(integer()) :: float()
  def yield(cars_per_hour) do
    raise "Not Yet Implemented"
  end

  @spec working_cars_per_hour(integer()) :: integer()
  def working_cars_per_hour(cars_per_hour) do
    raise "Not Yet Implemented"
  end

  @spec oee_measurement(float(), float(), float()) :: float()
  def oee_measurement(availability, performance, quality) do
    raise "Not Yet Implemented"
  end
end