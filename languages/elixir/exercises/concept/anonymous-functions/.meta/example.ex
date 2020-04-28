defmodule ClosureMaker do
  def make_adder(number) do
    fn x -> x + number end
  end

  def make_subtractor(number) do
    fn x -> x - number end
  end

  def make_multiplier(number) do
    fn a -> a * number end
  end

  def make_divider(number) do
    fn x -> div(x, number) end
  end

  def make_list_appender(list) do
    fn l -> l ++ list end
  end

  def make_string_appender(string) do
    fn s -> s <> string end
  end
end
