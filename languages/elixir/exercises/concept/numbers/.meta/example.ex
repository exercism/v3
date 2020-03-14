defmodule AssemblyLine do

  @spec success_rate(integer()) :: float()
  defp success_rate(speed) do
    cond do
      speed >= 9 -> 0.77
      speed >= 5 -> 0.9
      true       -> 1.0
    end
  end

  @spec production_rate_per_hour(integer()) :: float()
  def production_rate_per_hour(speed) do
    221 * speed * success_rate(speed)
  end

  @spec working_items_per_minute(integer()) :: integer()
  def working_items_per_minute(speed) do
    (production_rate_per_hour(speed) / 60) |> trunc()
  end
end