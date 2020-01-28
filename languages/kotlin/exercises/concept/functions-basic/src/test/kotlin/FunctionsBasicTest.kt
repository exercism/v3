import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Test

class FunctionsBasicTest {

    @Test
    fun `default value`() {
        assertEquals(1, increment(0))
        assertEquals(Int::class, increment(0)::class)
        assertEquals(11, increment(10))
    }

    @Test
    fun `zero increment`() {
        assertEquals(8, increment(8, 0))
        assertEquals(-9, increment(-9, 0))
    }

    @Test
    fun `positive increment`() {
        assertEquals(15, increment(10, 5))
        assertEquals(112, increment(100, 12))
    }

    @Test
    fun `negative increment`() {
        assertEquals(7, increment(10, -3))
        assertEquals(-2, increment(0, -2))
    }

    @Test
    fun `negative value`() {
        assertEquals(0, increment(-1))
        assertEquals(-1, increment(-2))
        assertEquals(-3, increment(-10, 7))
        assertEquals(8, increment(-2, 10))
    }

    @Test
    fun `extreme values`() {
        assertEquals(Int.MIN_VALUE, increment(Int.MAX_VALUE))
        assertEquals(Int.MAX_VALUE, increment(Int.MIN_VALUE, -1))
        assertEquals(-1, increment(Int.MIN_VALUE, Int.MAX_VALUE))
    }
}
