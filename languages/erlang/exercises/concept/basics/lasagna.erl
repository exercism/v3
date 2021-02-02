-module(lasagna).
-export([expected_minutes_in_oven/0, 
         remaining_minutes_in_oven/1,
         preparation_time_in_minutes/1,
         total_time_in_minutes/2]).

expected_minutes_in_oven() ->
  "Please implement the expected_minutes_in_oven/0 function".

remaining_minutes_in_oven(ActualMinutesInOven) ->
  "Please implement the remaining_minutes_in_oven/1 function".

preparation_time_in_minutes(NumberOfLayers) ->
  "Please implement the preparation_time_in_minutes/1 function".

total_time_in_minutes(NumberOfLayers, ActualMinutesInOven) ->
  "Please implement the total_time_in_minutes/2 function".
