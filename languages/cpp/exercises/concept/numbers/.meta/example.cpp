#include "numbers.h"

namespace assembly_line {

constexpr std::int32_t production_rate_default_speed = 221;

namespace {
double success_rate(std::int32_t speed) {
    if (speed == 10) {
        return 0.77;
    }

    if (speed == 9) {
        return 0.8;
    }

    if (speed >= 5) {
        return 0.9;
    }

    return 1;
}
}  // namespace

double production_rate_per_hour(std::int32_t speed) {
    return production_rate_default_speed * speed * success_rate(speed);
}

std::int32_t working_items_per_minute(std::int32_t speed) {
    return production_rate_per_hour(speed) / 60;
}

}  // namespace assembly_line
