defmodule CaptainsLog do
  @habitable_planetary_classes ["H", "K", "L", "M", "N", "R"]

  def random_habitable_planet() do
    Enum.random(@habitable_planetary_classes)
  end

  def random_stardate() do
    :rand.uniform() * 3000 + 2000
  end

  def format_stardate(stardate) do
    to_string(:io_lib.format(".1f", [stardate]))
  end
end
