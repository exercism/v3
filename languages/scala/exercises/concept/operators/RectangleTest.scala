/** @version 1.3.0 */
class RectangleTest extends FunSuite with Matchers {

  test("plus operator for rectangles of same height") {
    Rectangle(2, 5) + Rectangle(4, 5) should be Rectangle (6, 5)
  }

  test("plus operator for rectangles of different height") {
    pending
    Rectangle(1, 1) + Rectangle(2, 2) should be Rectangle (3, 2)
  }

  test("multiply operator for rectangles with factor 1") {
    pending
    Rectangle(2, 3) * 1 should be Rectangle (2, 3)
  }

  test("multiply operator for rectangles with factor 5") {
    pending
    Rectangle(2, 3) * 5 should be Rectangle (10, 15)
  }
}
