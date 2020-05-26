defmodule NameBadge do
  def print(id, name, nil) do
    print(id, name, "owner")
  end

  def print(id, name, department) do
    badge = "#{name} - #{String.upcase(department)}"
    prefix = if id, do: "[#{id}] - ", else: ""

    prefix <> badge
  end
end
