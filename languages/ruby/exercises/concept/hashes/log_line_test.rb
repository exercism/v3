# frozen_string_literal: true

require 'minitest/autorun'
require_relative 'log_line'

class LogLineTest < MiniTest::Test
  def test_parse_error
    assert_equal LogLevel::ERROR, LogLine.new('[ERROR]: Disk full').level
  end

  def test_parse_warning
    assert_equal LogLevel::WARNING, LogLine.new('[WARNING]: Timezone not set').level
  end

  def test_parse_info
    assert_equal LogLevel::INFO, LogLine.new('[INFO]: Timezone changed').level
  end

  def test_parse_unknown
    assert_equal LogLevel::UNKNOWN, LogLine.new('[FATAL]: Crash!').level
  end

  def test_short_output_error
    assert_equal "#{LogLevel::ERROR}:Stack overflow", LogLine.new('[ERROR]: Stack overflow').short
  end

  def test_short_output_warning
    assert_equal "#{LogLevel::WARNING}:Unsafe password", LogLine.new('[WARNING]: Unsafe password').short
  end

  def test_short_output_info
    assert_equal "#{LogLevel::INFO}:File moved", LogLine.new('[INFO]: File moved').short
  end

  def test_short_output_unknown
    assert_equal "#{LogLevel::UNKNOWN}:Something happened", LogLine.new('[FATAL]: Something happened').short
  end

  def test_display_available_levels
    assert_equal [0, 1, 2, 4], LogLine.available_levels
  end
end
