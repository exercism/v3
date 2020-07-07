#include <assert.h>
#include <inttypes.h>

int64_t expected_minutes_in_oven(void);
int64_t remaining_minutes_in_oven(int64_t actual_minutes_in_oven);
int64_t preparation_time_in_minutes(int64_t number_of_layers);
int64_t total_time_in_minutes(int64_t number_of_layers, int64_t actual_minutes_in_oven);

int main(void) {
    assert(expected_minutes_in_oven() == 40);
    assert(remaining_minutes_in_oven(25) == 15);
    assert(preparation_time_in_minutes(1) == 2);
    assert(preparation_time_in_minutes(4) == 8);
    assert(total_time_in_minutes(1, 30) == 32);
    assert(total_time_in_minutes(4, 8) == 16);
}
