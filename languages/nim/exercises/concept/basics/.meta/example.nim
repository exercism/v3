const expectedMinutesInOven* = 40

const preparationMinutesPerLayer = 2 ##\
  ## The amount of minutes it takes to prepare a single layer.

func remainingMinutesInOven*(actualMinutesInOven: int) =
  ## Determine the amount of minutes the lasagna still needs to remain in the
  ## oven to be properly prepared.
  expectedMinutesInOven - actualMinutesInOven

func preparationTimeInMinutes*(numberOfLayers: int) =
  ## Given a number of layers, determine the total preparation time.
  numberOfLayers * preparationMinutesPerLayer

func totalTimeInMinutes*(numberOfLayers, actualMinutesInOven: int) =
  ## Calculate the total working time. That is, the time to prepare all the layers
  ## of lasagna, and the time already spent in the oven.
  preparationTimeInMinutes(numberOfLayers) + actualMinutesInOven
