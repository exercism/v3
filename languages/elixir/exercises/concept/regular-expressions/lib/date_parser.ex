defmodule DateParser do
  def month(), do: "\\d{1,2}"
  def day(), do: "\\d{1,2}"
  def year(), do: "\\d{4}"

  @days ~w[Sunday Monday Tuesday Wednesday Thursday Friday Saturday]
  def day_names() do
    "(#{Enum.join(@days, "|")})"
  end

  @months ~w[January February March April May June July August September October November December]
  def month_names() do
    "(#{Enum.join(@months, "|")})"
  end

  def capture_month(), do: "(?<month>#{month()})"
  def capture_day(), do: "(?<day>#{day()})"
  def capture_year(), do: "(?<year>#{year()})"

  def capture_month_names(), do: "(?<month_names>#{month_names()})"
  def capture_day_names(), do: "(?<day_names>#{day_names()})"

  def capture_numeric_date(), do: "#{capture_day()}/#{capture_month()}/#{capture_year()}"
  def capture_month_name_date(), do: "#{capture_month_names()} #{capture_day()}, #{capture_year()}"
  def capture_day_month_name_date(), do: "#{capture_day_names()}, #{capture_month_name_year()}"

  def match_numeric_date(), do: ~r'^#{capture_numeric_date()}$'
  def match_month_name_date(), do: ~r'^#{capture_month_name_date()}$'
  def match_day_month_name_date(), do: ~r'^#{capture_day_month_name_date()}$'
end
