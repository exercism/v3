#include "numbers.h"

#include "test/catch.hpp"

TEST_CASE("Production rate per hour for speed zero") {
    auto rate = assembly_line::production_rate_per_hour(0);
    static_assert(std::is_same<double, decltype(rate)>::value,
                  "Returned value should be of type double");
    REQUIRE(0.0 == rate);
}

#if defined(EXERCISM_RUN_ALL_TESTS)
TEST_CASE("Production rate_per_hour_for_speed_one") {
    auto rate = assembly_line::production_rate_per_hour(1);
    static_assert(std::is_same<double, decltype(rate)>::value,
                  "Returned value should be of type double");
    REQUIRE(221.0 == rate);
}

TEST_CASE("Production rate_per_hour_for_speed_four") {
    auto rate = assembly_line::production_rate_per_hour(4);
    static_assert(std::is_same<double, decltype(rate)>::value,
                  "Returned value should be of type double");
    REQUIRE(884.0 == rate);
}

TEST_CASE("Production rate_per_hour_for_speed_seven") {
    auto rate = assembly_line::production_rate_per_hour(7);
    static_assert(std::is_same<double, decltype(rate)>::value,
                  "Returned value should be of type double");
    REQUIRE(1392.3 == rate);
}

TEST_CASE("Working items per minute for speed zero") {
    auto rate = assembly_line::working_items_per_minute(0);
    static_assert(std::is_same<int32_t, decltype(rate)>::value,
                  "Returned value should be of type int32_t");
    REQUIRE(0 == rate);
}

TEST_CASE("Working items per minute for speed one") {
    auto rate = assembly_line::working_items_per_minute(1);
    static_assert(std::is_same<int32_t, decltype(rate)>::value,
                  "Returned value should be of type int32_t");
    REQUIRE(3 == rate);
}

TEST_CASE("Working items per minute for speed eight") {
    auto rate = assembly_line::working_items_per_minute(8);
    static_assert(std::is_same<int32_t, decltype(rate)>::value,
                  "Returned value should be of type int32_t");
    REQUIRE(26 == rate);
}

TEST_CASE("Working items per minute for speed nine") {
    auto rate = assembly_line::working_items_per_minute(9);
    static_assert(std::is_same<int32_t, decltype(rate)>::value,
                  "Returned value should be of type int32_t");
    REQUIRE(26 == rate);
}

TEST_CASE("Working items per minute for speed ten") {
    auto rate = assembly_line::working_items_per_minute(10);
    static_assert(std::is_same<int32_t, decltype(rate)>::value,
                  "Returned value should be of type int32_t");
    REQUIRE(28 == rate);
}
#endif
