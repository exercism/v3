func remainingMinutesInOven(elapsedMinutes: Int, expectedMinutesInOven: Int = 40) -> Int {
  guard elapsedMinutes < expectedMinutesInOven else { return 0 }
  return expectedMinutesInOven - elapsedMinutes
}

func preparationTimeInMinutes(layers: String...) -> Int {
  layers.count * 2
}

func quantities(layers: String...) -> (noodles: Int, sauce: Double) {
  var quantity = (noodles: 0, sauce: 0.0)
  for layer in layers {
    if layer == "noodles" {
      quantity.noodles += 3
    } else if layer == "sauce" {
      quantity.sauce += 0.2
    }
  }
  return quantity
}

func toOz(_ amount: inout (noodles: Int, sauce: Double)) {
  amount = (noodles: amount.noodles, sauce: amount.sauce * 33.814)
}

func redWine(layers: String...) -> Bool {
  func countMozzarella() -> Int {
    var count = 0
    for layer in layers {
      if layer == "mozzarella" { count += 1 }
    }
    return count
  }
  func countRicotta() -> Int {
    var count = 0
    for layer in layers {
      if layer == "ricotta" { count += 1 }
    }
    return count
  }
  func countBechamel() -> Int {
    var count = 0
    for layer in layers {
      if layer == "béchamel" { count += 1 }
    }
    return count
  }
  func countMeat() -> Int {
    var count = 0
    for layer in layers {
      if layer == "meat" { count += 1 }
    }
    return count
  }
  func countSauce() -> Int {
    var count = 0
    for layer in layers {
      if layer == "sauce" { count += 1 }
    }
    return count
  }

  return countMozzarella() + countRicotta() + countBechamel() < countMeat() + countSauce()

}
