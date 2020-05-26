-module(lasagna_tests).
-include_lib("eunit/include/eunit.hrl").

expected_minutes_in_oven_test() ->
  ?assertEqual(40, lasagna:expected_minutes_in_oven()).

remaining_minutes_in_oven_test() ->
  ?assertEqual(15, lasagna:remaining_minutes_in_oven(25)).

preparation_time_in_minutes_test() ->
  ?assertEqual(2, lasagna:preparation_time_in_minutes(1)),
  ?assertEqual(8, lasagna:preparation_time_in_minutes(4)).

total_time_in_minutes_test() ->
  ?assertEqual(32, lasagna:total_time_in_minutes(1, 30)),
  ?assertEqual(16, lasagna:total_time_in_minutes(4, 8)).
