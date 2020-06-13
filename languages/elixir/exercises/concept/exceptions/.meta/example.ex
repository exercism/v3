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

defmodule RPNCalculator.Output do
  def write(resource, filename, equation) do
    {:ok, file} = resource.open(filename)

    try do
      IO.write(file, equation)
    rescue
      _ -> {:error, "Unable to write to resource"}
    else
      :ok -> {:ok, equation}
    after
      resource.close(file)
    end
  end
end
