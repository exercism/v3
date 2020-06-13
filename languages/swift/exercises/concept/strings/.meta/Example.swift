import Foundation

func splitOnNewlines(_ poem: String) -> [String] {
  return poem.components(separatedBy: "\n")
}

func firstLetter(_ line: String) -> Character {
  return line.first ?? "_"
}

func capitalize(_ phrase: String) -> String {
  return phrase.capitalized
}

func trimFromEnd(_ line: String) -> String {
  return line.trimmingCharacters(in: .whitespaces)
}

func lastLetter(_ line: String) -> Character {
  return line.last ?? "_"
}

func backDoorPassword(_ phrase: String) -> String {
  return "\(phrase.capitalized), please"
}
