defmodule LogLevelTest do
  use ExUnit.Case

  test "label trace" do
    assert LogLevel.to_label(0) == :trace
  end

  test "label debug" do
    assert LogLevel.to_label(1) == :debug
  end

  test "label info" do
    assert LogLevel.to_label(4) == :info
  end

  test "label warning" do
    assert LogLevel.to_label(5) == :warning
  end

  test "label error" do
    assert LogLevel.to_label(6) == :error
  end

  test "label fatal" do
    assert LogLevel.to_label(7) == :fatal
  end

  test "label unknown" do
    assert LogLevel.to_label(10) == :unknown
  end

  test "label another unknown" do
    assert LogLevel.to_label(-1) == :unknown
  end
end
