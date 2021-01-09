defmodule KitchenCalculator do
  # Get the number component from the volume-unit pair '{:unit, volume}'

  def get_volume(volume_pair), do: elem(volume_pair, 1)

  # Convert to milliliter to another unit

  def to_milliliter({:cup, cups}) do
    {:milliliter, cups * 240}
  end

  def to_milliliter({:fluid_ounce, floz}) do
    {:milliliter, floz * 30}
  end

  def to_milliliter({:teaspoon, teaspoons}) do
    {:milliliter, teaspoons * 5}
  end

  def to_milliliter({:tablespoon, tablespoons}) do
    {:milliliter, tablespoons * 15}
  end

  def to_milliliter({:milliliter, _} = volume) do
    volume
  end

  # Convert from milliliter to another unit

  def from_milliliter({:milliliter, mls}, :cup) do
    {:cup, mls / 240}
  end

  def from_milliliter({:milliliter, mls}, :fluid_ounce) do
    {:fluid_ounce, mls / 30}
  end

  def from_milliliter({:milliliter, mls}, :teaspoon) do
    {:teaspoon, mls / 5}
  end

  def from_milliliter({:milliliter, mls}, :tablespoon) do
    {:tablespoon, mls / 15}
  end

  def from_milliliter({:milliliter, _} = volume, :milliliter) do
    volume
  end

  # convert from a supported unit to a supported unit

  def convert(volume_pair, to) do
    to_milliliter(volume_pair) |> from_milliliter(to)
  end
end
