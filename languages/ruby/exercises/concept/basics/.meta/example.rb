class Lasagna
  EXPECTED_MINUTES_IN_OVEN = 40
  PREPERATION_MINUTES_PER_LAYER = 2

  def remaining_minutes_in_oven(actual_minutes_in_oven)
    expected_minutes_in_oven - actual_minutes_in_oven
  end

  def preperation_time_in_minutes(number_of_layers)
    number_of_layers * PREPERATION_MINUTES_PER_LAYER
  end

  def total_time_in_minutes(number_of_layers:, actual_minutes_in_oven:)
    preparation_time_in_minutes(number_of_layers) + actual_minutes_in_oven
  end
end
