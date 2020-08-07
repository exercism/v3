let birthday = "Birthday"
let valentine = "Valentine's Day"
let anniversary = "Anniversary"

let space: Character = " "
let exclamation: Character = "!"

func buildSign(for occasion: String, name: String) -> String {
  return "Happy " + occasion + String(space) + name + String(exclamation)
}

func graduationFor(name: String, year: Int) -> String {
  "Congratulations \(name)!\nClass of \(year)"
}

func numberOfLines(in message: String) -> Int {
  var newlineCount = 1
  for char in message {
    if char == "\n" { newlineCount += 1 }
  }
  return newlineCount
}

func numberOfNumbers(in message: String) -> Int {
  var numberCount = 0
  for char in message {
    if char.isNumber { numberCount += 1 }
  }
  return numberCount
}
