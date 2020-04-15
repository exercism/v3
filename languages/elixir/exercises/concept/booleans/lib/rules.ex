defmodule Rules do
  def eat_ghost?(power_pellet_active, touching_ghost) do
    raise "eat_ghost?/2 not yet implemented"
  end

  def score?(touching_power_pellet, touching_power_pellet) do
    raise "score?/2 not yet implemented"
  end

  def lose?(power_pellet_active, touching_ghost) do
    raise "lose?/2 not yet implemented"
  end

  def win?(has_eaten_all_dots, power_pellet_active, touching_ghost) do
    raise "win?/3 not yet implemented"
  end
end
