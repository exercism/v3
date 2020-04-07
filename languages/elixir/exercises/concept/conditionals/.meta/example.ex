defmodule LogLevel do
  def to_label(level) do
    cond do
      level === 0 -> :trace
      level === 1 -> :debug
      level === 4 -> :info
      level === 5 -> :warning
      level === 6 -> :error
      level === 7 -> :fatal
      true -> :unknown
    end
  end
end
