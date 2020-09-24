import org.junit.Assert;
import org.junit.Test;

public class PlayAnalyzerTest {

    @Test(expected = IllegalArgumentException.class)
    public void test_exception(){
        PlayAnalyzer.analyseOnField(13);
    }

    @Test(expected = IllegalArgumentException.class)
    public void test_exception_negative_number(){
        PlayAnalyzer.analyseOnField(-1);
    }

    @Test
    public void test_goal(){
        Assert.assertEquals("goalie", PlayAnalyzer.analyseOnField(1));
    }

    @Test
    public void test_left_back(){
        Assert.assertEquals("left back", PlayAnalyzer.analyseOnField(2));
    }

    @Test
    public void test_right_back(){
        Assert.assertEquals("right back", PlayAnalyzer.analyseOnField(5));
    }

    @Test
    public void test_center_back(){
        Assert.assertEquals("center back", PlayAnalyzer.analyseOnField(3));
        Assert.assertEquals("center back", PlayAnalyzer.analyseOnField(4));
    }

    @Test
    public void test_midfielder(){
        Assert.assertEquals("midfielder", PlayAnalyzer.analyseOnField(6));
        Assert.assertEquals("midfielder", PlayAnalyzer.analyseOnField(7));
        Assert.assertEquals("midfielder", PlayAnalyzer.analyseOnField(8));
    }

    @Test
    public void test_left_wing(){
        Assert.assertEquals("left wing", PlayAnalyzer.analyseOnField(9));
    }

    @Test
    public void test_striker(){
        Assert.assertEquals("striker", PlayAnalyzer.analyseOnField(10));
    }

    @Test
    public void test_right_wing(){
        Assert.assertEquals("right wing", PlayAnalyzer.analyseOnField(11));
    }
}
