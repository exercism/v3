/** @version 1.3.0 */
class LasagnaTest extends FunSuite with Matchers {

  test("expected minutes in oven") {
    new Lasagna().expectedMinutesInOven() should be(40)
  }

  test("remaining minutes in oven after 25 minutes") {
    pending
    new Lasagna().remainingMinutesInOven(25) should be(15)
  }

  test("preparation time in minutes for one layer") {
    pending
    new Lasagna().preparationTimeInMinutes(1) should be(2)
  }

  test("preparation time in minutes for multiple layers") {
    pending
    new Lasagna().preparationTimeInMinutes(4) should be(8)
  }

  test("elapsed time in minutes for one layer") {
    pending
    new Lasagna().elapsedTimeInMinutes(1, 30) should be(32)
  }

  test("elapsed time in minutes for multiple layers") {
    pending
    new Lasagna().elapsedTimeInMinutes(4, 8) should be(16)
  }
}
