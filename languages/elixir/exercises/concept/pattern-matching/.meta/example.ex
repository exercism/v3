defmodule DateString do

  def from_tuple({{year, month, day}, {hour, minute, second}}) do
    "#{year}-#{month}-#{day} #{hour}:#{minute}:#{second}"
  end

  def from_map(
        %{
          year: year,
          month: month,
          day: day,
          hour: hour,
          minute: minute,
          second: second,
        }
      ) do
    "#{year}-#{month}-#{day} #{hour}:#{minute}:#{second}"
  end
end
