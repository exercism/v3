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

func costOf(sign: String) -> Int {
  sign.count * 2 + 20
}
