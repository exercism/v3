#include "numbers.h"

namespace assembly_line {

constexpr uint32_t PRODUCTION_RATE_DEFAULT_SPEED = 221;

namespace {
double success_rate(const int32_t speed) {
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

double production_rate_per_hour(const int32_t speed) {
    return PRODUCTION_RATE_DEFAULT_SPEED * speed * success_rate(speed);
}

int32_t working_items_per_minute(const int32_t speed) {
    return production_rate_per_hour(speed) / 60;
}

}  // namespace assembly_line
