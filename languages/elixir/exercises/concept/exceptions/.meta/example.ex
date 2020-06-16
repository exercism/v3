defmodule RPNCalculator.Exception do
  defmodule DivisionByZeroError do
    defexception message: "division by zero occurred"
  end

  defmodule StackUnderflowError do
    defexception message: "stack underflow occurred"

    @impl true
    def exception(value) do
      error = %StackUnderflowError{}
      case value do
        [] -> error
        _ ->  %{error | message: "#{error.message}, context: #{value}"}
      end
    end
  end
end
