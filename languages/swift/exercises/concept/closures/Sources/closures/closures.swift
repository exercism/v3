func unroll(_ seed: Int,
            generator f: (Int) -> (seed: Int, value: Int),
            stop done: (Int) -> Bool
          ) -> [Int] {
  func unfold(_ accumulator: [Int], _ seed: Int) -> [Int] {
    guard !done(seed) else { return accumulator }
    let next = f(seed)
    return unfold(accumulator + [next.value], next.seed)
  }
  return unfold([], seed)
}

//let generateDigit = TODO: Please define the generateDigit closure
//
//let stopAtZero = TODO: Please define the stopAtZero closure
//
//
//let digitsOf = TODO: Please define the digitsOf closure
//
//
//let generateBit = TODO: Please define the generateBit closure
//
//
//let bitsOf = TODO: Please define the bitsOf closure

func generatorForBase(_ base: Int) -> ((Int) -> (seed: Int, next: Int))? {
  fatalError("Please implement the generatorForBase(_:) closure")
}

//let generateCollatz = TODO: Please define the generateCollatz closure
//
//
//let stopAtOne = TODO: Please define the stopAtOne closure
//
//
//let collatz = TODO: Please define the collatz closure
