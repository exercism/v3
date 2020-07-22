import org.junit.Before;
import org.junit.Test;
import org.junit.Ignore;


import static org.junit.Assert.assertEquals;

public class BirdCountTest {

    private BirdCount birdCount;
    private int lastWeek[] = {0, 2, 5, 3, 7, 8, 4};

    @Before
    public void setUp() {
        birdCount = new BirdCount(lastWeek);
    }

    @Test
    public void itTestGetLastWeek() {
        assertEquals(true, birdCount.getLastWeek().equals(lastWeek));
    }

    @Test
    public void itTestGetToday() {
        assertEquals(lastWeek[lastWeek.length - 1], birdCount.getToday());
    }

    @Test
    public void itShouldReturnZeroIfBirdCountLastWeekIsEmpty() {
        int[] lastWeekEmpty = new int[0];
        birdCount = new BirdCount(lastWeekEmpty);
        assertEquals(0, birdCount.getToday());
    }

    @Test
    public void itIncrementTodaysCount() {
        int currentTodayCount = birdCount.getToday();
        birdCount.incrementTodaysCount();
        assertEquals(currentTodayCount + 1, birdCount.getToday());
    }

    @Test
    public void itHasDayWithoutBirds() {
        assertEquals(true, birdCount.hasDayWithoutBirds());
    }

    @Test
    @Ignore
    public void itShouldNotHaveDaysWithoutBirds() {
        birdCount = new BirdCount(new int[]{1, 2, 5, 3, 7, 8, 4});
        assertEquals(false, birdCount.hasDayWithoutBirds());
    }


    @Test
    public void itTestGetCountForFirstDays() {
        assertEquals(10, birdCount.getCountForFirstDays(4));
    }

    @Test
    @Ignore
    public void itTestGetCountForMoreDaysThanTheArraySize() {
        assertEquals(29, birdCount.getCountForFirstDays(10));
    }

    @Test
    public void itTestGetCountForBusyDays() {
        assertEquals(3, birdCount.getBusyDays());
    }

    @Test
    @Ignore
    public void itShouldNotHaveBusyDays() {
        birdCount = new BirdCount(new int[]{1, 2, 3, 3, 2, 1, 4});
        assertEquals(0, birdCount.getBusyDays());
    }
}
