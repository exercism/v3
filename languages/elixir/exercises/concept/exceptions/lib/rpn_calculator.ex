defmodule RPNCalculator.Exception do
  defmodule DivisionByZeroError do
    raise "Please implement the DivisionByZeroError exception"
  end

  defmodule StackUnderflowError do
    raise "Please implement the StackUnderflowError exception"
  end
end

defmodule RPNCalculator.Output do
  def write(resource, filename, equation) do
    raise "Please implement the write/3 function"
  end
end
