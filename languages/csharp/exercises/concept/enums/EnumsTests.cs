using Xunit;

public class LogLineTests
{
    [Fact]
    public void ParseError() =>
        Assert.Equal(LogLevel.Error, LogLine.ParseLogLevel("[ERR]: Disk full"));

    [Fact]
    public void ParseWarning() =>
        Assert.Equal(LogLevel.Warning, LogLine.ParseLogLevel("[WRN]: Timezone not set"));

    [Fact]
    public void ParseInfo() =>
        Assert.Equal(LogLevel.Info, LogLine.ParseLogLevel("[INF]: Timezone changed"));

    [Fact]
    public void ParseUnknown() =>
        Assert.Equal(LogLevel.Unknown, LogLine.ParseLogLevel("[XYZ]: Crash!"));

    [Fact]
    public void OutputForShortLogForError() =>
        Assert.Equal("6:Stack overflow", LogLine.OutputForShortLog(LogLevel.Error, "Stack overflow"));

    [Fact]
    public void OutputForShortLogForWarning() =>
        Assert.Equal("5:Unsafe password", LogLine.OutputForShortLog(LogLevel.Warning, "Unsafe password"));

    [Fact]
    public void OutputForShortLogForInfo() =>
        Assert.Equal("4:File moved", LogLine.OutputForShortLog(LogLevel.Info, "File moved"));

    [Fact]
    public void OutputForShortLogForUnknown() =>
        Assert.Equal("42:Something unknown happened", LogLine.OutputForShortLog(LogLevel.Unknown, "Something unknown happened"));
}