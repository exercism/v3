defmodule Monopoly do
  def d6 do
    1..6
    |> Enum.shuffle()
    |> Enum.at(0)
  end

  def roll() do
    fn -> {d6(), d6()} end
    |> Stream.repeatedly()
    |> Stream.with_index()
    |> Stream.transform(false, fn {{d1, d2}, index}, acc ->
      if index == 0 || (acc && index < 3) do
        {[{d1, d2}], d1 == d2}
      else
        {:halt, false}
      end
    end)
  end
end
