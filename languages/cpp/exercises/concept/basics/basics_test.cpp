#include "basics.h"
#include "test/catch.hpp"

using namespace std;

TEST_CASE("Expected minutes in oven") {
  const int actual = Lasagna().expectedMinutesInOven();
  const int expected = 40;

  REQUIRE(expected == actual);
}

#if defined(EXERCISM_RUN_ALL_TESTS)
TEST_CASE("Remaining minutes in oven") {
  const int actual = Lasagna().remainingMinutesInOven(25);

  const int expected = 15;

  REQUIRE(actual == expected);
}

TEST_CASE("Preparation time in minutes for one layer") {
  const int actual = Lasagna().preparationTimeInMinutes(1);

  const int expected = 2;

  REQUIRE(actual == expected);
}

TEST_CASE("Preparation time in minutes for multiple layers") {
  const int actual = Lasagna().preparationTimeInMinutes(4);

  const int expected = 8;

  REQUIRE(actual == expected);
}

TEST_CASE("Elapsed time in minutes for one layer") {
  const int actual = Lasagna().elapsedTimeInMinutes(1, 30);

  const int expected = 32;

  REQUIRE(actual == expected);
}

TEST_CASE("Elapsed time in minutes for multiple layers") {
  const int actual = Lasagna().elapsedTimeInMinutes(4, 8);

  const int expected = 16;

  REQUIRE(actual == expected);
}
#endif
