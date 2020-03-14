defmodule AssemblyLineTest do
  use ExUnit.Case

  test "production rate per hour for speed 0" do
    rate = AssemblyLine.production_rate_per_hour(0)
    assert is_float(rate) and rate == 0.0
  end

  test "production rate per hour for speed 1" do
    rate = AssemblyLine.production_rate_per_hour(1)
    assert is_float(rate) and rate == 221.0
  end

  test "production rate per hour for speed 4" do
    rate = AssemblyLine.production_rate_per_hour(4)
    assert is_float(rate) and rate == 884.0
  end

  test "production rate per hour for speed 7" do
    rate = AssemblyLine.production_rate_per_hour(7)
    assert is_float(rate) and rate == 1392.3
  end

  test "production rate per hour for speed 9" do
    rate = AssemblyLine.production_rate_per_hour(9)
    assert is_float(rate) and rate == 1531.53
  end

  test "working items per minute for speed 0" do
    item_count = AssemblyLine.working_items_per_minute(0)
    assert is_integer(item_count) and item_count == 0
  end

  test "working items per minute for speed 1" do
    item_count = AssemblyLine.working_items_per_minute(1)
    assert is_integer(item_count) and item_count == 3
  end

  test "working items per minute for speed 5" do
    item_count = AssemblyLine.working_items_per_minute(5)
    assert is_integer(item_count) and item_count == 16
  end

  test "working items per minute for speed 8" do
    item_count = AssemblyLine.working_items_per_minute(8)
    assert is_integer(item_count) and item_count == 26
  end

  test "working items per minute for speed 10" do
    item_count = AssemblyLine.working_items_per_minute(10)
    assert is_integer(item_count) and item_count == 28
  end
end
