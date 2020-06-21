defmodule TakeANumberTest do
  use ExUnit.Case

  # @tag :pending
  test "starts a new process" do
    pid = TakeANumber.start()
    assert is_pid(pid)
    assert pid != self()
    assert pid != TakeANumber.start()
  end

  @tag :pending
  test "reports its own state" do
    pid = TakeANumber.start()
    send(pid, {:report_state, self()})
    assert_receive 0
  end

  @tag :pending
  test "does not shut down after reporting its own state" do
    pid = TakeANumber.start()
    send(pid, {:report_state, self()})
    assert_receive 0

    send(pid, {:report_state, self()})
    assert_receive 0
  end

  @tag :pending
  test "gives out a number" do
    pid = TakeANumber.start()
    send(pid, {:take_a_number, self()})
    assert_receive 1
  end

  @tag :pending
  test "gives out many consecutive numbers" do
    pid = TakeANumber.start()
    send(pid, {:take_a_number, self()})
    assert_receive 1

    send(pid, {:take_a_number, self()})
    assert_receive 2

    send(pid, {:take_a_number, self()})
    assert_receive 3

    send(pid, {:report_state, self()})
    assert_receive 3

    send(pid, {:take_a_number, self()})
    assert_receive 4

    send(pid, {:take_a_number, self()})
    assert_receive 5

    send(pid, {:report_state, self()})
    assert_receive 5
  end

  @tag :pending
  test "stops" do
    pid = TakeANumber.start()
    assert Process.alive?(pid)
    send(pid, {:report_state, self()})
    assert_receive 0

    send(pid, :stop)
    send(pid, {:report_state, self()})
    refute_receive 0
    refute Process.alive?(pid)
  end

  @tag :pending
  test "keeps working after receiving an unexpected message" do
    pid = TakeANumber.start()

    send(pid, :hello?)
    send(pid, "I want to speak with the manager")

    send(pid, {:take_a_number, self()})
    assert_receive 1

    send(pid, {:report_state, self()})
    assert_receive 1
  end
end
