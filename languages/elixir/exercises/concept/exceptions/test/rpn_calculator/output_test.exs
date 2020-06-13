defmodule RPNCalculator.OutputTest do
  use ExUnit.Case

  setup_all do
    {:ok, pid} = MockResource.start()
    [mock_resource: pid]
  end

  setup do
    on_exit(fn ->
      MockResource.set_test_failure(false)
      MockResource.close_all()
      MockResource.clear_log()
    end)
  end

  describe "write/3" do
    @tag :pending
    @filename "filename"
    @equation "1 1 +"
    @use_open_error_message """
    Use the open/1 function from the `resource` specified in the arguments to open `filename`.

    E.g.) resource.open(filename)
    """
    test "opens the resource" do
      RPNCalculator.Output.write(MockResource, @filename, @equation)

      with log <- MockResource.get_log(),
           true <- length(log) > 0,
           open_entry <- Enum.at(log, 0),
           true <- String.starts_with?(open_entry, "[OPEN]"),
           msg <- String.trim_leading(open_entry, "[OPEN]") |> String.trim(),
           opened_file <- msg |> String.split(":", parts: 2) |> hd(),
           true <- opened_file == @filename do
        assert true
      else
        _ ->
          flunk(@use_open_error_message)
      end
    end

    @tag :pending
    @filename "filename"
    @equation "1 1 +"
    @use_write_error_message """
    Use IO.write/2 to write to the opened `filename`.
    """
    test "then writes to the resource" do
      RPNCalculator.Output.write(MockResource, @filename, @equation)

      with log <- MockResource.get_log(),
           true <- length(log) > 0,
           msg_entry <- Enum.at(log, 1),
           true <- String.starts_with?(msg_entry, "[MSG]"),
           equation <- String.trim_leading(msg_entry, "[MSG]") |> String.trim(),
           true <- equation == @equation do
        assert true
      else
        _ ->
          flunk(@use_write_error_message)
      end
    end

    @tag :pending
    @filename "filename"
    @equation "1 1 +"
    @use_close_error_message """
    Use the close/1 function from the `resource` specified in the arguments to close the opened file handle.

    E.g.) resource.close(filename)
    """
    test "then closes the resource" do
      RPNCalculator.Output.write(MockResource, @filename, @equation)

      with log <- MockResource.get_log(),
           true <- length(log) == 3,
           msg_entry <- Enum.at(log, 2),
           true <- String.starts_with?(msg_entry, "[CLOSE]") do
        assert true
      else
        _ ->
          flunk(@use_close_error_message)
      end
    end

    @tag :pending
    @filename "filename"
    @equation "1 1 +"
    test "returns error tuple when error raised on write" do
      MockResource.set_test_failure(true)

      assert {:error, "Unable to write to resource"} =
               RPNCalculator.Output.write(MockResource, @filename, @equation)
    end

    @tag :pending
    @filename "filename"
    @equation "1 1 +"
    test "returns ok tuple when success on write" do
      assert {:ok, @equation} = RPNCalculator.Output.write(MockResource, @filename, @equation)
    end

    @tag :pending
    @filename "filename"
    @equation "1 1 +"
    test "closes the resource even when it errors" do
      MockResource.set_test_failure(true)

      assert {:error, "Unable to write to resource"} =
               RPNCalculator.Output.write(MockResource, @filename, @equation)

      with log <- MockResource.get_log(),
           true <- length(log) == 1,
           close_entry <- hd(log),
           true <- String.starts_with?(close_entry, "[CLOSE]") do
        assert true
      else
        _ ->
          flunk("write/3 should close the `resource` even if an error is raised")
      end
    end
  end
end
