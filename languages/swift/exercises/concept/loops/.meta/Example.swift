func hashIDs(_ ids: [Int]) -> [(Int, Int)] {
  let m = 3_999_999_979
  let p = 2_305_843_009
  var result = [(Int, Int)]()
  for id in ids {
    guard 1..<m ~= id else { continue }
    result.append(($0, $0 * p % m))
  }
  return result
}

func digitalSum(_ number: Int) -> Int {
  guard number > 0 else { return 0 }
  var num = number
  var sum = 0
  repeat {
    sum += num % 10
    num /= 10
  } while num > 0
  return sum
}

func rankingLevel(hashedID id: Int) -> Int {
  guard id > 0 else { return 0 }
  guard id > 1 else { return 1 }
  var current = id
  var level = 0
  while current.isMultiple(of: digitalSum(current)) {
    level += 1
    let sum = digitalSum(current)
    guard sum > 1 else { break }
    current /= sum
  }
  return level
}

func rankIDs(hashedIDs: [(Int, Int)]) -> [(Int, Int)] {
  var result = [(Int, Int)]()
  for (id, hash) in hashedIDs {
    result.append((id, rankingLevel(hash)))
  }
  return result
}
