using Xunit;

// TODO: convert Theory-based tests to Fact-based tests.
// This is necessary in order to be able to display the 
// input for which the test failed, which is defined in
// the .meta/config.json file
public class LogLineTest
{
    [Theory]
    [InlineData("[ERROR]: Stack overflow", LogLevel.Error)]
    [InlineData("[ERROR]: Disk full", LogLevel.Error)]
    [InlineData("[ERROR]: Segmentation fault", LogLevel.Error)]
    [InlineData("[ERROR]: File not found", LogLevel.Error)]
    [InlineData("[WARNING]: Disk almost full", LogLevel.Warning)]
    [InlineData("[WARNING]: Timezone not set", LogLevel.Warning)]
    [InlineData("[WARNING]: Unsafe password", LogLevel.Warning)]
    [InlineData("[INFO]: File moved", LogLevel.Info)]
    [InlineData("[INFO]: Timezone changed", LogLevel.Info)]
    public void Parse(string logLine, LogLevel expected) =>
        Assert.Equal(expected, LogLine.ParseLogLevel(logLine));

    [Theory]
    [InlineData("[UNKNOWN]: Something unknown happened", LogLevel.Unknown)]
    [InlineData("[FATAL]: Crash!", LogLevel.Unknown)]
    public void ParseUnknown(string logLine, LogLevel expected) =>
        Assert.Equal(expected, LogLine.ParseLogLevel(logLine));

    [Theory]
    [InlineData(LogLevel.Error, "Stack overflow", "4:Stack overflow")]
    [InlineData(LogLevel.Error, "Disk full", "4:Disk full")]
    [InlineData(LogLevel.Error, "Segmentation fault", "4:Segmentation fault")]
    [InlineData(LogLevel.Error, "File not found", "4:File not found")]
    [InlineData(LogLevel.Warning, "Disk almost full", "2:Disk almost full")]
    [InlineData(LogLevel.Warning, "Timezone not set", "2:Timezone not set")]
    [InlineData(LogLevel.Warning, "Unsafe password", "2:Unsafe password")]
    [InlineData(LogLevel.Info, "File moved", "1:File moved")]
    [InlineData(LogLevel.Info, "Timezone changed", "1:Timezone changed")]
    [InlineData(LogLevel.Unknown, "Something unknown happened", "0:Something unknown happened")]
    [InlineData(LogLevel.Unknown, "Crash!", "0:Crash!")]
    public void OutputForShortLog(LogLevel logLevel, string message, string expected) =>
        Assert.Equal(expected, LogLine.OutputForShortLog(logLevel, message));
}