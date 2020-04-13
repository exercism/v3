defmodule Rules do
  def eat_ghost?(power_pellet_active, touching_ghost) do
    power_pellet_active and touching_ghost
  end

  def score?(ate_power_pellet, ate_dot) do
    ate_power_pellet or ate_dot
  end

  def lose?(power_pellet_active, touching_ghost) do
    not power_pellet_active and touching_ghost
  end

  def win?(ate_all_dots, power_pellet_active, touching_ghost) do
    ate_all_dots and not lose?(power_pellet_active, touching_ghost)
  end
end
