func getCard(at index: Int, from stack: [Int]) -> Int {
  return stack[index]
}

func setCard(at index: Int, in stack: [Int], to newCard: Int) -> [Int] {
  var newStack = stack
  newStack[index] = newCard
  return newStack
}

func insert(_ newCard: Int, atTopOf stack: [Int]) -> [Int] {
  return stack + [newCard]
}

func removeCard(at index: Int, from stack: [Int]) -> [Int] {
  var newStack = stack
  newStack.remove(at: index)
  return newStack
}

func removeTopCard(_ stack: [Int]) -> [Int] {
  var newStack = stack
  newStack.removeLast()
  return newStack
}

func insert(_ newCard: Int, atBottomOf stack: [Int]) -> [Int] {
  var newStack = stack
  newStack.insert(newCard, at: 0)
  return newStack
}

func removeBottomCard(_ stack: [Int]) -> [Int] {
  var newStack = stack
  newStack.removeFirst()
  return newStack
}

func checkSizeOfStack(_ stack: [Int], _ size: Int) -> Bool {
  return stack.count == size
}
