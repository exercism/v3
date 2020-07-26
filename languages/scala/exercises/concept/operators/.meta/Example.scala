case class Rectangle(width: Int, height: Int) {
  def +(that: Rectangle): Rectangle =
    Rectangle(width + that.width, math.max(height, that.height))

  def *(factor: Int): Rectangle = Rectangle(width * factor, height * factor)
}
