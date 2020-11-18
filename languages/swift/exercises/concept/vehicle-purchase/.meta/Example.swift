func canIBuy(vehicle: String, price: Double, monthlyBudget: Double) -> String {
  if price / 60 < monthlyBudget {
    return "Yes! I'm getting a " + vehicle
  } else if price / 60 > monthlyBudget + 10 {
    return "Darn! No " + vehicle + " for me"
  } else {
    return "I'll have to be frugal if I want a " + vehicle
  }
}

func licenseType(numberOfWheels wheels: Int) -> String {
  switch wheels {
  case 2, 3: return "You will need a motorcycle license for your vehicle"
  case 4, 6: return "You will need an automobile license for your vehicle"
  case 18: return "You will need a commercial trucking license for your vehicle"
  default: return "We do not issue licenses for those types of vehicles"
  }
}

func registrationFee(msrp: Int, yearsOld: Int) -> Int {
  guard yearsOld < 10 else { return 25 }
  let value = max(msrp, 25000)
  let percent = 1.0 - Double(yearsOld) / 10.0  // 10 - yearsOld
  let fee = Double(value) * percent / 100.0
  return Int(fee)
}
