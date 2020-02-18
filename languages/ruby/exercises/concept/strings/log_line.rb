# frozen_string_literal: true

class LogLine
  def self.message(line)
    raise NotImplementedError, 'Please implement the LogLine.message method'
  end

  def self.log_level(line)
    raise NotImplementedError, 'Please implement the LogLine.log_level method'
  end

  def self.reformat(line)
    raise NotImplementedError, 'Please implement the LogLine.reformat method'
  end
end
