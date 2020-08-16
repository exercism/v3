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

let generateDigit = { (n: Int) -> (seed: Int, next: Int) in
  (seed: n / 10, next: n % 10)
}

let stopAtZero = { (n: Int) -> Bool in n == 0 }

let digitsOf = { (n: Int) -> [Int] in
  unroll(n, generator: generateDigit, stop: stopAtZero)
}

let generateBit = { (n: Int) -> (base: Int, value: Int) in
  (base: n / 2, value: n % 2)
}

let bitsOf = { (n: Int) -> [Int] in
  unroll(n, generator: generateBit, stop: stopAtZero)
}

func generatorForBase(_ base: Int) -> ((Int) -> (seed: Int, next: Int))? {
  guard 2...10 ~= base else { return nil }
  return { n in
    (base: n / base, next: n % base)
  }
}

let generateCollatz = { (n: Int) -> (base: Int, value: Int) in
  (base: n.isMultiple(of: 2) ? n / 2 : 3 * n + 1, value: n)
}

let stopAtOne = { (n: Int) -> Bool in n == 1 }

let collatz = { (n: Int) -> [Int] in
  guard n > 0 else { return [] }
  return unroll(n, generator: collatzGenerator, stop: stopAtOne)
}
