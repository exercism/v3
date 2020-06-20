defmodule MonopolyTest do
  use ExUnit.Case

  @iterations 1000

  describe "d6/0" do
    test "returns an integer from 1 to 6" do
      assert 1..@iterations
             |> Enum.map(fn _ -> Monopoly.d6() end)
             |> Enum.all?(&(&1 in 1..6))
    end
  end

  describe "roll/0" do
    test "returns a stream" do
      assert is_function(Monopoly.roll())
      assert is_list(Enum.to_list(Monopoly.roll()))
    end

    test "returns a list of tuples with two d6 rolls" do
      assert [roll1 | _] = Enum.to_list(Monopoly.roll())
      assert {d6_1, d6_2} = roll1
      assert d6_1 in 1..6
      assert d6_2 in 1..6
    end

    test "stops after one roll if rolled two different values" do
      assert 1..@iterations
             |> Enum.map(fn _ -> Enum.to_list(Monopoly.roll()) end)
             |> Enum.all?(fn rolls ->
               if Enum.count(rolls) == 1 do
                 [{d6_1, d6_2}] = rolls
                 d6_1 != d6_2
               else
                 true
               end
             end)
    end

    test "stops after two rolls if rolled two different values on the second roll" do
      assert 1..@iterations
             |> Enum.map(fn _ -> Enum.to_list(Monopoly.roll()) end)
             |> Enum.all?(fn rolls ->
               if Enum.count(rolls) == 2 do
                 [{roll1_d6_1, roll1_d6_2}, {roll2_d6_1, roll2_d6_2}] = rolls
                 roll1_d6_1 == roll1_d6_2 && roll2_d6_1 != roll2_d6_2
               else
                 true
               end
             end)
    end

    test "stops after three rolls regardless of the third roll's result" do
      assert 1..@iterations
             |> Enum.map(fn _ -> Enum.to_list(Monopoly.roll()) end)
             |> Enum.all?(fn rolls ->
               case Enum.count(rolls) do
                 n when n > 3 ->
                   false

                 3 ->
                   [{roll1_d6_1, roll1_d6_2}, {roll2_d6_1, roll2_d6_2}, _] = rolls
                   roll1_d6_1 == roll1_d6_2 && roll2_d6_1 == roll2_d6_2

                 _ ->
                   true
               end
             end)
    end
  end
end
