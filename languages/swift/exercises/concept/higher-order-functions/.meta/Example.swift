func generateCombination(forRoom room: Int, usingFunction f: (Int) -> Int) -> (Int, Int, Int) {
  let a = f(room)
  let b = f(a)
  let c = f(b)
  return (a, b, c)
}

func protectSecret(_ secret: String, withPassword password: String) -> (String) -> String {
  func protected(_ pass: String) -> String {
    if pass == password {
      return secret
    } else {
      return "Sorry. No hidden secrets here."
    }
  }
  return protected
}
