import org.junit.Assert;
import org.junit.Test;

public class PlayAnalyzerTest {

    @Test
    public void test_goal(){
        Assert.assertEquals("goalie", PlayAnalyzer.onField(1));
    }

    @Test
    @Ignore("Remove to run test")
    public void test_left_back(){
        Assert.assertEquals("left back", PlayAnalyzer.onField(2));
    }

    @Test
    @Ignore("Remove to run test")
    public void test_right_back(){
        Assert.assertEquals("right back", PlayAnalyzer.onField(5));
    }

    @Test
    @Ignore("Remove to run test")
    public void test_center_back(){
        Assert.assertEquals("center back", PlayAnalyzer.onField(3));
        Assert.assertEquals("center back", PlayAnalyzer.onField(4));
    }

    @Test
    @Ignore("Remove to run test")
    public void test_midfielder(){
        Assert.assertEquals("midfielder", PlayAnalyzer.onField(6));
        Assert.assertEquals("midfielder", PlayAnalyzer.onField(7));
        Assert.assertEquals("midfielder", PlayAnalyzer.onField(8));
    }

    @Test
    @Ignore("Remove to run test")
    public void test_left_wing(){
        Assert.assertEquals("left wing", PlayAnalyzer.onField(9));
    }

    @Test
    @Ignore("Remove to run test")
    public void test_striker(){
        Assert.assertEquals("striker", PlayAnalyzer.onField(10));
    }

    @Test
    @Ignore("Remove to run test")
    public void test_right_wing(){
        Assert.assertEquals("right wing", PlayAnalyzer.onField(11));
    }

    @Test(expected = IllegalArgumentException.class)
    @Ignore("Remove to run test")
    public void test_exception(){
        PlayAnalyzer.onField(13);
    }

    @Test(expected = IllegalArgumentException.class)
    @Ignore("Remove to run test")
    public void test_exception_negative_number(){
        PlayAnalyzer.onField(-1);
    }
}
