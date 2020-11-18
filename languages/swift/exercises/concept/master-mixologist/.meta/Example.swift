func timeToPrepare(drinks: [String]) -> Double {
  func timeFor(_ drink: String) -> Double {
    switch drink {
    case "beer", "soda", "water": return 0.5
    case "shot": return 1.0
    case "mixed drink": return 1.5
    case "fancy drink": return 2.5
    case "frozen drink": return 3.0
    default: return 0
    }
  }
  var time = 0.0
  for drink in drinks {
    time += timeFor(drink)
  }
  return time
}

func makeWedges(needed: Int, limes: [String]) -> Int {
  func wedgesFrom(_ lime: String) -> Int {
    switch lime {
    case "small": return 6
    case "medium": return 8
    case "large": return 10
    default: return 0
    }
  }
  var stillNeed = needed
  var limes = limes
  var limesUsed = 0
  while stillNeed > 0 && !limes.isEmpty {
    stillNeed -= wedgesFrom(limes.removeFirst())
    limesUsed += 1
  }
  return limesUsed
}

func finishShift(minutesLeft: Int, remainingOrders: [[String]]) -> [[String]] {
  var timeLeft = Double(minutesLeft)
  var orders = remainingOrders
  repeat {
    timeLeft -= timeToPrepare(drinks: orders.removeFirst())
  } while timeLeft > 0 && !orders.isEmpty
  return orders
}

func orderTracker(orders: [(drink: String, time: String)])
  -> (
    beer: (first: String, last: String, total: Int)?,
    soda: (first: String, last: String, total: Int)?
  )
{
  var beerStats: (first: String, last: String, total: Int)? = nil
  var sodaStats: (first: String, last: String, total: Int)? = nil
  for (drink, time) in orders {
    switch drink {
    case "beer":
      if beerStats == nil {
        beerStats = (first: time, last: time, total: 1)
      } else {
        beerStats?.last = time
        beerStats?.total += 1
      }
    case "soda":
      if sodaStats == nil {
        sodaStats = (first: time, last: time, total: 1)
      } else {
        sodaStats?.last = time
        sodaStats?.total += 1
      }
    default: continue
    }
  }
  return (beer: beerStats, soda: sodaStats)
}
