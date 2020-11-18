enum LogLevel: Int {
  case trace
  case debug
  case info = 4
  case warning
  case error
  case fatal
  case unknown = 42

  init(_ line: String) {
    guard let lead = line.first, lead == "[" else {
      self = .unknown
      return
    }
    switch line.dropFirst().prefix(3) {
    case "TRC": self = .trace
    case "DBG": self = .debug
    case "INF": self = .info
    case "WRN": self = .warning
    case "ERR": self = .error
    case "FTL": self = .fatal
    default: self = .unknown
    }
  }

  func shortFormat(message: String) -> String {
    "\(self.rawValue):\(message)"
  }
}
