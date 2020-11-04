let flip = { (tuple: (String, String, String)) -> (String, String, String) in
  let (a, b, c) = tuple
  return (b, a, c)
}

let rotate = { (tuple: (String, String, String)) -> (String, String, String) in
  let (a, b, c) = tuple
  return (b, c, a)
}

func makeShuffle(
  flipper: @escaping ((String, String, String)) -> (String, String, String),
  rotator: @escaping ((String, String, String)) -> (String, String, String)
) -> (UInt8, (String, String, String)) -> (String, String, String) {
  { (key: UInt8, wires: (String, String, String)) -> (String, String, String) in
    var bits = key
    var order = wires
    for _ in 1...8 {
      if bits.isMultiple(of: 2) {
        order = flipper(order)
      } else {
        order = rotator(order)
      }
      bits /= 2
    }
    return order
  }
}
