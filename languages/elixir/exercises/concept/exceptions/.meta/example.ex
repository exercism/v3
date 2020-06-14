defmodule RPNCalculator.Exception do
  defmodule DivisionByZeroError do
    defexception message: "division by zero occurred"
  end

  defmodule StackUnderflowError do
    defexception [:message]

    @impl true
    def exception(operation_name) do
      msg = "encountered stack underflow with the #{operation_name} operation"
      %StackUnderflowError{message: msg}
    end
  end
end
