defmodule GuessingGameTest do
  use ExUnit.Case
  doctest GuessingGame

  describe "responds with correct when the guessed number equals " do
    test "7" do
      assert GuessingGame.compare(7, 7) == "Correct"
    end
  end

  describe "responds with too high when guessed number is greater than" do
    test "7" do
      assert GuessingGame.compare(7, 14) == "Too High"
    end
  end

  describe "responds with too low when guessed number is less than" do
    test "7" do
      assert GuessingGame.compare(7, 3) == "Too Low"
    end
  end

  describe "responds with so close when guess differs from 7" do
    test "by -1" do
      assert GuessingGame.compare(7, 6) == "So close"
    end

    test "by +1" do
      assert GuessingGame.compare(7, 8) == "So close"
    end
  end

  test "when no guess is supplied, ask the player to make a guess" do
    assert GuessingGame.compare(7) == "Make a guess"
  end
end
