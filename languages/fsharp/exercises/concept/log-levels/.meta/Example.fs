module Strings

let message (logLine: string): string = logLine.Substring(logLine.IndexOf(':') + 1).Trim()

let logLevel (logLine: string): string = logLine.Substring(1, logLine.IndexOf(']') - 1).ToLower()

let reformat (logLine: string): string = sprintf "%s (%s)" (message logLine) (logLevel logLine)
