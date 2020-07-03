#include <assert.h>
#include <inttypes.h>

int64_t expected_minutes_in_oven(void);
int64_t remaining_minutes_in_oven(int64_t actual_minutes_in_oven);
int64_t preparation_time_in_minutes(int64_t number_of_layers);
int64_t elapsed_time_in_minutes(int64_t number_of_layers, int64_t actual_minutes_in_oven);

int main(void) {
    assert(40 == expected_minutes_in_oven());
    assert(15 == remaining_minutes_in_oven(25));
    assert(2 == preparation_time_in_minutes(1));
    assert(8 == preparation_time_in_minutes(4));
    assert(32 == elapsed_time_in_minutes(1, 30));
    assert(16 == elapsed_time_in_minutes(4, 8));
}
