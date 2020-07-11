class Lasagna {

  def remainingMinutesInOven(actualMinutesInOven: Int): Int =
    expectedMinutesInOven() - actualMinutesInOven

  def expectedMinutesInOven(): Int = 40

  def elapsedTimeInMinutes(numberOfLayers: Int, actualMinutesInOven: Int): Int =
    preparationTimeInMinutes(numberOfLayers) + actualMinutesInOven

  def preparationTimeInMinutes(numberOfLayers: Int): Int = numberOfLayers * 2

}
