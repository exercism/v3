expected_minutes_in_oven <- function() {
  60
}

remaining_time_in_minutes <- function(minutes) {
  expected_minutes_in_oven() - minutes
}

prep_time_in_minutes <- function(layers) {
  2 * layers
}

elapsed_time_in_minutes <- function(layers, minutes) {
  prep_time_in_minutes(layers) + minutes
}
