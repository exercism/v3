defmodule KitchenCalculatorTest do
  use ExUnit.Case

  alias KitchenCalculator, as: KC

  describe "get volume from tuple pair" do
    test "get cups" do
      assert KC.get_volume({:cup, 1}) == 1
    end

    test "get fluid ounces" do
      assert KC.get_volume({:fluid_ounce, 2}) == 2
    end

    test "get teaspoons" do
      assert KC.get_volume({:teaspoons, 3}) == 3
    end

    test "get tablespoons" do
      assert KC.get_volume({:tablespoons, 4}) == 4
    end

    test "get millilitres" do
      assert KC.get_volume({:millilitre, 5}) == 5
    end
  end

  describe "convert to millilitres from" do
    test "millilitres" do
      assert KC.to_millilitre({:millilitre, 3}) == {:millilitre, 3}
    end

    test "cups" do
      assert KC.to_millilitre({:cup, 3}) == {:millilitre, 720}
    end

    test "fluid ounces" do
      assert KC.to_millilitre({:fluid_ounce, 100}) == {:millilitre, 3000}
    end

    test "teaspoon" do
      assert KC.to_millilitre({:teaspoon, 3}) == {:millilitre, 15}
    end

    test "tablespoon" do
      assert KC.to_millilitre({:tablespoon, 3}) == {:millilitre, 45}
    end
  end

  describe "convert from millilitres to" do
    test "millilitres" do
      assert KC.from_millilitre({:millilitre, 4}, :millilitre) == {:millilitre, 4}
    end

    test "cups" do
      assert KC.from_millilitre({:millilitre, 840}, :cup) == {:cup, 3.5}
    end

    test "fluid ounces" do
      assert KC.from_millilitre({:millilitre, 4522.5}, :fluid_ounce) == {:fluid_ounce, 150.75}
    end

    test "teaspoon" do
      assert KC.from_millilitre({:millilitre, 61.25}, :teaspoon) == {:teaspoon, 12.25}
    end

    test "tablespoon" do
      assert KC.from_millilitre({:millilitre, 71.25}, :tablespoon) == {:tablespoon, 4.75}
    end
  end

  describe "convert from x to y:" do
    test "teaspoon to tablespoon" do
      assert KC.convert({:teaspoon, 15}, :tablespoon) == {:tablespoon, 5}
    end

    test "cups to fluid ounces" do
      assert KC.convert({:cup, 4}, :fluid_ounce) == {:fluid_ounce, 32}
    end

    test "fluid ounces to teapoons" do
      assert KC.convert({:fluid_ounce, 4}, :teaspoon) == {:teaspoon, 24}
    end

    test "tablespoons to cups" do
      assert KC.convert({:tablespoon, 320}, :cup) == {:cup, 20}
    end
  end
end
