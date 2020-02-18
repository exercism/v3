# frozen_string_literal: true

require 'minitest/autorun'
require_relative 'log_line'

class LogLineTest < Minitest::Test
  def test_error_message
    assert_equal 'Stack overflow', LogLine.message('[ERROR]: Stack overflow')
  end

  def test_warning_message
    assert_equal 'Disk almost full', LogLine.message('[WARNING]: Disk almost full')
  end

  def test_info_message
    assert_equal 'File moved', LogLine.message('[INFO]: File moved')
  end

  def test_message_with_leading_and_trailing_space
    assert_equal 'Timezone not set', LogLine.message("[WARNING]:   \tTimezone not set  \r\n")
  end

  def test_error_log_level
    assert_equal 'error', LogLine.log_level('[ERROR]: Disk full')
  end

  def test_warning_log_level
    assert_equal 'warning', LogLine.log_level('[WARNING]: Unsafe password')
  end

  def test_info_log_level
    assert_equal 'info', LogLine.log_level('[INFO]: Timezone changed')
  end

  def test_erro_reformat
    assert_equal 'Segmentation fault (error)', LogLine.reformat('[ERROR]: Segmentation fault')
  end

  def test_warning_reformat
    assert_equal 'Decreased performance (warning)', LogLine.reformat('[WARNING]: Decreased performance')
  end

  def test_info_reformat
    assert_equal 'Disk defragmented (info)', LogLine.reformat('[INFO]: Disk defragmented')
  end

  def rest_reformat_with_leading_and_trailing_space
    assert_equal 'Corrupt disk (error)', LogLine.reformat("[ERROR]: \t Corrupt disk\t \t \r\n")
  end
end
