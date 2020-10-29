func swapAt<T>(_ array: inout [T], _ x: Int, _ y: Int) {
  guard x < array.count && y < array.count else { return }
  let temp = array[x]
  array[x] = array[y]
  array[y] = temp
  return
}

func bubbleSort<T>(_ array: inout [T], swapIf: (T, T) -> Bool) {
  var wasSwapped = false
  for last in (1..<array.endIndex).reversed() {
    for x in 0..<last {
      let y = x + 1
      if swapIf(array[x], array[y]) {
        swapAt(&array, x, y)
        wasSwapped = true
      }
    }
    if !wasSwapped { break }
    wasSwapped = false
  }
}
