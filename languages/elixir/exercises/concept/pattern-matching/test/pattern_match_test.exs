defmodule DateStringTest do
  use ExUnit.Case

  @tag :pending # remove line to run test
  test "converts date tuple to string" do
    assert "2020-12-31 23:59:58" = DateString.from_tuple({{2020, 12, 31}, {23, 59, 58}})
    assert "2020-04-24 01:09:17" = DateString.from_tuple({{2020, 4, 24}, {1, 9, 17}})
  end

  @tag :pending # remove line to run test
  test "matches on maps" do
    assert "2020-12-31 23:59:58" = DateString.from_map(%{
             calendar: Calendar.ISO,
             day: 31,
             hour: 23,
             microsecond: {143104, 6},
             minute: 59,
             month: 12,
             second: 58,
             std_offset: 0,
             time_zone: "Etc/UTC",
             utc_offset: 0,
             year: 2020,
             zone_abbr: "UTC"
           })
    assert "2020-04-24 01:09:17" = DateString.from_map(%{
             calendar: Calendar.ISO,
             day: 24,
             hour: 1,
             microsecond: {143104, 6},
             minute: 9,
             month: 4,
             second: 17,
             std_offset: 0,
             time_zone: "Etc/UTC",
             utc_offset: 0,
             year: 2020,
             zone_abbr: "UTC"
           })
  end
end
