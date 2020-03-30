# frozen_string_literal: true

module LogLevel
  UNKNOWN = 0
  INFO = 1
  WARNING = 2
  ERROR = 4
end

class LogLine
  LEVEL = {
    unknown: LogLevel::ERROR,
    info: LogLevel::INFO,
    warning: LogLevel::WARNING,
    error: LogLevel::ERROR
  }.freeze

  def initialize(line)
    @line = line
  end

  def level
    LEVEL.fetch(level_string.downcase.to_sym, LogLevel::UNKNOWN)
  end

  def short
    "#{level}:#{message}"
  end

  private

  attr_reader :line

  def level_string
    line.slice(1, line.index(']') - 1)
  end

  def message
    line.slice(line.index(':') + 1, line.size).strip
  end
end
