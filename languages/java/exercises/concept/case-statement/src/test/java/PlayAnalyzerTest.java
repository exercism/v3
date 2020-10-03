import org.junit.Assert;
import org.junit.Test;

public class PlayAnalyzerTest {

    @Test(expected = IllegalArgumentException.class)
    public void test_exception(){
        PlayAnalyzer.onField(13);
    }

    @Test(expected = IllegalArgumentException.class)
    public void test_exception_negative_number(){
        PlayAnalyzer.onField(-1);
    }

    @Test
    public void test_goal(){
        Assert.assertEquals("goalie", PlayAnalyzer.onField(1));
    }

    @Test
    public void test_left_back(){
        Assert.assertEquals("left back", PlayAnalyzer.onField(2));
    }

    @Test
    public void test_right_back(){
        Assert.assertEquals("right back", PlayAnalyzer.onField(5));
    }

    @Test
    public void test_center_back(){
        Assert.assertEquals("center back", PlayAnalyzer.onField(3));
        Assert.assertEquals("center back", PlayAnalyzer.onField(4));
    }

    @Test
    public void test_midfielder(){
        Assert.assertEquals("midfielder", PlayAnalyzer.onField(6));
        Assert.assertEquals("midfielder", PlayAnalyzer.onField(7));
        Assert.assertEquals("midfielder", PlayAnalyzer.onField(8));
    }

    @Test
    public void test_left_wing(){
        Assert.assertEquals("left wing", PlayAnalyzer.onField(9));
    }

    @Test
    public void test_striker(){
        Assert.assertEquals("striker", PlayAnalyzer.onField(10));
    }

    @Test
    public void test_right_wing(){
        Assert.assertEquals("right wing", PlayAnalyzer.onField(11));
    }
}
