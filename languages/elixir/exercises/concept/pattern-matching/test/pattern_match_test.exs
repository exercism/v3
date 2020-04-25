defmodule PatternMatchTest do
  use ExUnit.Case

  @tag :pending # remove line to run test
  test "converts date tuple to string" do
    assert "2020-12-31 23:59:59" = PatternMatch.date_tuple_to_string({{2020, 12, 31}, {23, 59, 59}})
    assert "2020-04-24 20:01:17" = PatternMatch.date_tuple_to_string({{2020, 4, 24}, {20, 1, 17}})
  end

  @tag :pending # remove line to run test
  test "second item from list" do
    assert "2020-12-31 23:59:59" = PatternMatch.date_list_to_string([2020, 12, 31, 23, 59, 59, 239])
    assert "2020-04-24 20:01:17" = PatternMatch.date_list_to_string([2020, 4, 24, 20, 1, 17, 98])
  end

  @tag :pending # remove line to run test
  test "matches on maps" do
    assert "2020-12-31 23:59:59" = PatternMatch.date_from_map(%{
             calendar: Calendar.ISO,
             day: 31,
             hour: 23,
             microsecond: {143104, 6},
             minute: 59,
             month: 12,
             second: 59,
             std_offset: 0,
             time_zone: "Etc/UTC",
             utc_offset: 0,
             year: 2020,
             zone_abbr: "UTC"
           })
    assert "2020-04-24 20:01:17" = PatternMatch.date_from_map(%{
             calendar: Calendar.ISO,
             day: 24,
             hour: 20,
             microsecond: {143104, 6},
             minute: 1,
             month: 4,
             second: 17,
             std_offset: 0,
             time_zone: "Etc/UTC",
             utc_offset: 0,
             year: 2020,
             zone_abbr: "UTC"
           })
  end

  @tag :pending # remove line to run test
  test "matches on fixed-length ASCII string" do
    assert "2020-12-31 23:59:59" = PatternMatch.parse_date_string("Year: 2020 Month: 12 Day: 31 Hour: 23 Minute: 59 Second: 59")
    assert "2020-04-24 20:01:17" = PatternMatch.parse_date_string("Year: 2020 Month: 04 Day: 24 Hour: 20 Minute: 01 Second: 17")
  end
end
