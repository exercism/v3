defmodule FileSniffer do
  def type_from_extension(extension) do
    raise "Please implement type_from_extension/1"
  end

  def type_from_header(file_binary) do
    raise "Please implement type_from_header/1"
  end

  def verify(file_binary, extension) do
    raise "Please implement verify/2"
  end
end
