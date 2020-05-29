defmodule BasketballWebsiteTest do
  use ExUnit.Case
  doctest BasketballWebsite

  test "greets the world" do
    assert BasketballWebsite.hello() == :world
  end
end
