using Xunit;

public class LogLineTest
{
    [Theory]
    [InlineData("[ERROR]: Stack overflow", "Stack overflow")]
    [InlineData("[ERROR]: Disk full", "Disk full")]
    [InlineData("[ERROR]: Segmentation fault", "Segmentation fault")]
    [InlineData("[ERROR]:  File not found ", "File not found")]
    [InlineData("[WARNING]: Disk almost full", "Disk almost full")]
    [InlineData("[WARNING]: \tTimezone not set\r\n", "Timezone not set")]
    [InlineData("[WARNING]: Unsafe password", "Unsafe password")]
    [InlineData("[INFO]: File moved", "File moved")]
    [InlineData("[INFO]: Timezone changed ", "Timezone changed")]
    public void Message(string logLine, string expected) =>
        Assert.Equal(expected, LogLine.Message(logLine));

    [Theory]
    [InlineData("[ERROR]: Stack overflow", "error")]
    [InlineData("[ERROR]: Disk full", "error")]
    [InlineData("[ERROR]: Segmentation fault", "error")]
    [InlineData("[ERROR]:  File not found ", "error")]
    [InlineData("[WARNING]: Disk almost full", "warning")]
    [InlineData("[WARNING]: \tTimezone not set\r\n", "warning")]
    [InlineData("[WARNING]: Unsafe password", "warning")]
    [InlineData("[INFO]: File moved", "info")]
    [InlineData("[INFO]: Timezone changed ", "info")]
    public void LogLevel(string logLine, string expected) =>
        Assert.Equal(expected, LogLine.LogLevel(logLine));

    [Theory]
    [InlineData("[ERROR]: Stack overflow", "Stack overflow (error)")]
    [InlineData("[ERROR]: Disk full", "Disk full (error)")]
    [InlineData("[ERROR]: Segmentation fault", "Segmentation fault (error)")]
    [InlineData("[ERROR]:  File not found ", "File not found (error)")]
    [InlineData("[WARNING]: Disk almost full", "Disk almost full (warning)")]
    [InlineData("[WARNING]: \tTimezone not set\r\n", "Timezone not set (warning")]
    [InlineData("[WARNING]: Unsafe password", "Unsafe password (warning)")]
    [InlineData("[INFO]: File moved", "File moved (info)")]
    [InlineData("[INFO]: Timezone changed ", "Timezone changed (info)")]
    public void Reformat(string logLine, string expected) =>
        Assert.Equal(expected, LogLine.Reformat(logLine));
}