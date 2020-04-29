# frozen_string_literal: true

module LogLevel
  UNKNOWN = 0
  INFO = 1
  WARNING = 2
  ERROR = 4
end

class LogLine
  attr_reader :line

  LEVELS = {
    unknown: LogLevel::UNKNOWN,
    info: LogLevel::INFO,
    warning: LogLevel::WARNING,
    error: LogLevel::ERROR
  }.freeze

  def self.available_levels
    LEVELS.map { |key, value| value }
  end

  def initialize(line)
    @line = line
  end

  def level
    LEVELS.fetch(level_string.downcase.to_sym, LogLevel::UNKNOWN)
  end

  def short
    "#{level}:#{message}"
  end

  def level_string
    line.slice(1, line.index(']') - 1)
  end

  def message
    line.slice(line.index(':') + 1, line.size).strip
  end
end
