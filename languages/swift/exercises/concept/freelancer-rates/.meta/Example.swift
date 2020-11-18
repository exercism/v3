let hoursPerDay = 8
let daysPerMonth = 22

func dailyRateFrom(hourlyRate: Int) -> Double {
  return Double(hourlyRate * hoursPerDay)
}

func monthlyRateFrom(hourlyRate: Int, withDiscount discount: Double) -> Double {
  let monthly = Double(hourlyRate * hoursPerDay * daysPerMonth) * (1.0 - (discount / 100))
  return monthly.rounded()
}

func workdaysIn(budget: Double, hourlyRate: Int, withDiscount discount: Double) -> Double {
  let fullMonthlyRate = budget / (1.0 - (discount / 100))
  let hours = fullMonthlyRate / Double(hourlyRate)
  let days = hours / Double(hoursPerDay)
  return days.rounded(.down)
}
