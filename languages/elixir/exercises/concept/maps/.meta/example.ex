defmodule HighScore do
  @initial_score 0

  def new(), do: %{}

  def add_player(scores, name) do
    Map.put(scores, name, @initial_score)
  end

  def remove_player(scores, name) do
    Map.delete(scores, name)
  end

  def update(scores, name, score) do
    Map.update(scores, name, @initial_score, &(&1 + score))
  end

  def reset(scores, name) do
    Map.put(scores, name, @initial_score)
  end

  def order_by_players(scores) do
    scores
    |> Map.keys()
    |> Enum.sort_by(&String.downcase/1)
  end

  def order_by_scores(scores) do
    scores
    |> Map.to_list()
    |> Enum.sort_by(&Kernel.elem(&1, 1))
  end
end
