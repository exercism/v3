func sliceSize(diameter: Double?, slices: Int?) -> Double? {
  guard
    let diameter = diameter,
    let slices = slices,
    diameter >= 0,
    slices > 0
  else { return nil }
  let radius = diameter / 2.0
  return Double.pi * radius * radius / Double(slices)
}

func biggestSlice(
  diameterA: String, slicesA: String,
  diameterB: String, slicesB: String
) -> String {
  let areaA = sliceSize(diameter: Double(diameterA), slices: Int(slicesA))
  let areaB = sliceSize(diameter: Double(diameterB), slices: Int(slicesB))

  switch (areaA, areaB) {
  case let (areaA, areaB) where areaA == areaB:
    return "Neither slice is bigger"
  case (nil, _):
    return "Slice B is bigger"
  case (_, nil):
    return "Slice A is bigger"
  case let (areaA?, areaB?):
    return areaA > areaB ? "Slice A is bigger" : "Slice B is bigger"
  }
}
