defmodule BasketballWebsite do
  @example1 %{ "coach" => %{ "name" => "Jane" }, "name" => "Hoop Masters" }
  def p1(), do: ".coach.name"
  def ex1(), do: @example1

  @example2 %{ "coaches" => [%{ "name" => "Jane" }], "name" => "Hoop Masters" }
  def p2(), do: ".coaches[0].name"
  def ex2(), do: @example1

  def extract(data, path) do
    paths = String.split(path, ".", trim: true)
    do_extract(data, paths)
  end

  def do_extract(nil, _), do: nil
  def do_extract(data, []), do: data
  def do_extract(data, [path|next]) do
    do_extract(data[path], next)
  end
end
