options = []
options = if Code.ensure_compiled(RPNCalculator.Exception.DivisionByZeroError) == {:error, :nofile}, do: [undefined_division_by_zero_error_module: true] ++ options, else: options
options = if Code.ensure_compiled(RPNCalculator.Exception.StackUnderflowError) == {:error, :nofile}, do: [undefined_stack_underflow_error_module: true] ++ options, else: options
options = if not function_exported?(RPNCalculator.Exception.DivisionByZeroError, :__struct__, 0), do: [undefined_struct_division_by_zero_error_exception: true] ++ options, else: options
options = if not function_exported?(RPNCalculator.Exception.StackUnderflowError, :__struct__, 0), do: [undefined_struct_stack_underflow_error_exception: true] ++ options, else: options
ExUnit.start(options)
ExUnit.configure(exclude: :pending, trace: true)
