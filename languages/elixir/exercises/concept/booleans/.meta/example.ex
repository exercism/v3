defmodule Rules do
  def eat_ghost?(ate_pill, touch_ghost) do
    ate_pill and touch_ghost
  end

  def score?(ate_pill, ate_dot) do
    ate_pill or ate_dot
  end

  def win?(ate_all_dots) do
    ate_all_dots
  end

  def lose?(ate_pill, touch_ghost) do
    not ate_pill and touch_ghost
  end
end
