#ifndef NUMBERS_H
#define NUMBERS_H

#include <cstdint>

namespace assembly_line {
double production_rate_per_hour(const int32_t speed);
int32_t working_items_per_minute(const int32_t speed);
}  // namespace assembly_line

#endif /* NUMBERS_H */
