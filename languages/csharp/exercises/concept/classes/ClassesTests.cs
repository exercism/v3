using Xunit;

public class RaceTests
{
    [Fact]
    public void DistanceCoveredWithoutRunning()
    {
        var runner = new Runner(10, 20);
        Assert.Equal(0, runner.DistanceCovered());
    }

    [Fact]
    public void DistanceCoveredAfterSingleRunEqualsSpeed()
    {
        var runner = new Runner(10, 7);
        runner.Run();
        Assert.Equal(7, runner.DistanceCovered());
    }

    [Fact]
    public void DistanceCoveredAfterMultipleRunsWithStaminaRemaining()
    {
        var runner = new Runner(20, 6);
        runner.Run();
        runner.Run();
        runner.Run();
        Assert.Equal(18, runner.DistanceCovered());
    }

    [Fact]
    public void DistanceCoveredAfterMultipleRunsWithNoStaminaRemaining()
    {
        var runner = new Runner(10, 8);
        runner.Run();
        runner.Run();
        runner.Run();
        Assert.Equal(16, runner.DistanceCovered());
    }

    [Fact]
    public void RecordHolderStartsWithNoDistanceCovered()
    {
        var recordHolder = Race.RecordHolder();
        Assert.Equal(0, recordHolder.DistanceCovered());
    }

    [Fact]
    public void RecordHolderHasCorrectSpeed()
    {
        var recordHolder = Race.RecordHolder();
        recordHolder.Run();
        Assert.Equal(15, recordHolder.DistanceCovered());
    }

    [Fact]
    public void RecordHolderHasCorrectStamina()
    {
        var recordHolder = Race.RecordHolder();
        recordHolder.Run();
        recordHolder.Run();
        recordHolder.Run();
        recordHolder.Run();
        recordHolder.Run();
        recordHolder.Run();
        Assert.Equal(75, recordHolder.DistanceCovered());
    }

    [Fact]
    public void ChallengerDoesNotBreakRecordWhenNoLapsHaveBeenRun()
    {
        var challenger = new Runner(15, 20);
        var race = new Race(challenger);
        Assert.False(race.RecordBroken());
    }

    [Fact]
    public void ChallengerOutpacesRecordHolderAfterOneLapBreaksRecords()
    {
        var challenger = new Runner(15, 20);
        var race = new Race(challenger);
        race.Run();
        Assert.True(race.RecordBroken());
    }

    [Fact]
    public void ChallengerStaminaDoesNotHoldOutAndDoesNotBreakRecord()
    {
        var challenger = new Runner(15, 20);
        var race = new Race(challenger);
        race.Run();
        race.Run();
        race.Run();
        race.Run();
        race.Run();
        Assert.False(race.RecordBroken());
    }
}