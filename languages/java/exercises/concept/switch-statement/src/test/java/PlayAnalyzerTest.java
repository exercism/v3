import org.junit.Ignore;
import org.junit.Test;

import static org.assertj.core.api.Assertions.*;

public class PlayAnalyserTest {

    @Test
    public void test_goal() {
        assertThat(PlayAnalyser.onField(1).contentEquals("goalie"));
    }

    @Test
    @Ignore("Remove to run test")
    public void test_left_back() {
        assertThat(PlayAnalyser.onField(2).contentEquals("left back"));
    }

    @Test
    @Ignore("Remove to run test")
    public void test_right_back() {
        assertThat(PlayAnalyser.onField(5).contentEquals("right back"));
    }

    @Test
    @Ignore("Remove to run test")
    public void test_center_back() {
        assertThat(PlayAnalyser.onField(3).contentEquals("center back"));
        assertThat(PlayAnalyser.onField(4).contentEquals("center back"));
    }

    @Test
    @Ignore("Remove to run test")
    public void test_midfielder() {
        assertThat(PlayAnalyser.onField(6).contentEquals("midfielder"));
        assertThat(PlayAnalyser.onField(7).contentEquals("midfielder"));
        assertThat(PlayAnalyser.onField(8).contentEquals("midfielder"));
    }

    @Test
    @Ignore("Remove to run test")
    public void test_left_wing() {
        assertThat(PlayAnalyser.onField(9).contentEquals("left wing"));
    }

    @Test
    @Ignore("Remove to run test")
    public void test_striker() {
        assertThat(PlayAnalyser.onField(10).contentEquals("striker"));
    }

    @Test
    @Ignore("Remove to run test")
    public void test_right_wing() {
        assertThat(PlayAnalyser.onField(11).contentEquals("right wing"));
    }

    @Test(expected = IllegalArgumentException.class)
    @Ignore("Remove to run test")
    public void test_exception() {
        PlayAnalyser.onField(13);
    }

    @Test(expected = IllegalArgumentException.class)
    @Ignore("Remove to run test")
    public void test_exception_negative_number() {
        PlayAnalyser.onField(-1);
    }
}
