defmodule RPNCalculator.ExceptionTest do
  use ExUnit.Case

  alias RPNCalculator.Exception.DivisionByZeroError
  alias RPNCalculator.Exception.StackUnderflowError

  test "DivisionByZeroError fields defined by `defexception`" do
    assert %DivisionByZeroError{}.__exception__ == true
    assert %DivisionByZeroError{}.__struct__ == RPNCalculator.Exception.DivisionByZeroError
    assert %DivisionByZeroError{}.message == "division by zero occurred"
  end

  @tag :pending
  test "DivisionByZeroError message when raised with raise/1" do
    assert_raise(DivisionByZeroError, "division by zero occurred", fn ->
      raise DivisionByZeroError
    end)
  end

  @tag :pending
  test "StackUnderflowError fields defined by `defexception`" do
    assert %StackUnderflowError{}.__exception__ == true
    assert %StackUnderflowError{}.__struct__ == RPNCalculator.Exception.StackUnderflowError
    assert %StackUnderflowError{}.message == nil
  end

  @tag :pending
  test "StackUnderflowError message when raised with raise/2" do
    assert_raise(StackUnderflowError, "encountered stack underflow with the test operation", fn ->
      raise StackUnderflowError, "test"
    end)
  end
end
