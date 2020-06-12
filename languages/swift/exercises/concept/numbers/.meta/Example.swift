let hoursPerDay = 8.0
let daysPerMonth = 22.0

func dailyRateFrom(hourlyRate: Double) -> Double {
  return hourlyRate * hoursPerDay
}

func monthlyRateFrom(hourlyRate: Double, withDiscount discount: Double) -> Double {
  let monthly = (hourlyRate * hoursPerDay * daysPerMonth) * (1.0 - (discount / 100))
  return monthly.rounded()
}

func workdaysIn(budget: Double, hourlyRate: Double, withDiscount discount: Double) -> Double {
  let fullMonthlyRate = budget / (1.0 - (discount / 100))
  let hours = fullMonthlyRate / hourlyRate
  let days = hours / hoursPerDay
  return days.rounded(.down)
}
