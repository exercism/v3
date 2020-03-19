defmodule AssemblyLine do

  @spec availability(integer()) :: float()
  def availability(minutes_worked) do
    days = 5
    hours = 8
    minutes_per_hour = 60
    max_uptime = days * hours * minutes_per_hour
    minutes_worked / max_uptime
  end

  @spec performance(integer()) :: float()
  def performance(cars_produced) do
    maximum_cars_per_hour = 15
    cars_produced / maximum_cars_per_hour
  end

  @spec yield(integer()) :: float()
  def yield(cars_per_hour) do
    cond do
      cars_per_hour >= 9 -> 0.77
      cars_per_hour >= 5 -> 0.9
      true       -> 1.0
    end
  end

  @spec working_cars_per_hour(integer()) :: integer()
  def working_cars_per_hour(cars_per_hour) do
    cars_per_hour |> yield() |> trunc()
  end

  @spec oee_measurement(float(), float(), float()) :: float()
  def oee_measurement(availability, performance, quality) do
    availability * performance * quality
  end
end