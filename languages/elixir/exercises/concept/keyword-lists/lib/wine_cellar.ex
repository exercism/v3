defmodule WineCellar do
  def explain_colors do
    raise "Please implement the explain_colors/0 function"
  end

  def filter(cellar, color, opts \\ []) do
    raise "Please implement the filter/3 function"
  end

  defp filter_by_year(_wines = [], _year), do: []

  defp filter_by_year([{_, year, _} = wine | tail], year) do
    [wine | filter_by_year(tail, year)]
  end

  defp filter_by_year([{_, _, _} | tail], year) do
    filter_by_year(tail, year)
  end

  defp filter_by_country(_wines = [], _country), do: []

  defp filter_by_country([{_, _, country} = wine | tail], country) do
    [wine | filter_by_country(tail, country)]
  end

  defp filter_by_country([{_, _, _} | tail], country) do
    filter_by_country(tail, country)
  end
end
