-module(lasagna).
-export([expected_minutes_in_oven/0, 
         remaining_minutes_in_oven/1,
         preparation_time_in_minutes/1,
         total_time_in_minutes/2]).

expected_minutes_in_oven() -> 40.

remaining_minutes_in_oven(ActualMinutesInOven) ->
  expected_minutes_in_oven() - ActualMinutesInOven.

preparation_time_in_minutes(NumberOfLayers) ->
  NumberOfLayers * 2.

total_time_in_minutes(NumberOfLayers, ActualMinutesInOven) ->
  preparation_time_in_minutes(NumberOfLayers) + ActualMinutesInOven.
