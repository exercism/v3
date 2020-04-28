defmodule ClosureMakerTest do
  use ExUnit.Case

  describe "make_adder" do
    test "add 3" do
      adder = ClosureMaker.make_adder(3)
      assert adder.(3) === 6
    end

    test "add 6" do
      adder = ClosureMaker.make_adder(6)
      assert adder.(9) === 15
    end
  end

  describe "make_subtractor" do
    test "subtract 3" do
      subtractor = ClosureMaker.make_subtractor(3)
      assert subtractor.(6) === 3
    end

    test "subtract 6" do
      subtractor = ClosureMaker.make_subtractor(6)
      assert subtractor.(3) === -3
    end
  end

  describe "make_multiplier" do
    test "multiply by 3" do
      multiplier = ClosureMaker.make_multiplier(3)
      assert multiplier.(6) === 18
    end

    test "multiply by 6" do
      multiplier = ClosureMaker.make_multiplier(6)
      assert multiplier.(7) === 42
    end
  end

  describe "make_divider" do
    test "divide by 3" do
      divider = ClosureMaker.make_divider(3)
      assert divider.(6) === 2
    end

    test "divide by 6" do
      divider = ClosureMaker.make_divider(6)
      assert divider.(7) === 1
    end
  end

  describe "make_list_appender" do
    test "append [3, 2, 1, \"Blast-off!\"]" do
      list = [3, 2, 1, "Blast-off!"]
      list_appender = ClosureMaker.make_list_appender(list)
      assert list_appender.([5, 4]) === [5, 4 | list]
    end

    test "append [6, 8, \"Who do we appreciate?\"]" do
      list = [6, 8, "Who do we appreciate?"]
      list_appender = ClosureMaker.make_list_appender(list)
      assert list_appender.([2, 4]) === [2, 4 | list]
    end
  end

  describe "make_string_appender" do
    test "append \"!\"" do
      suffix = "!"
      string_appender = ClosureMaker.make_string_appender(suffix)
      assert string_appender.("Hi") === "Hi!"
    end

    test "append \"?!\"" do
      suffix = "?!"
      string_appender = ClosureMaker.make_string_appender(suffix)
      assert string_appender.("WHAT") === "WHAT?!"
    end
  end
end
