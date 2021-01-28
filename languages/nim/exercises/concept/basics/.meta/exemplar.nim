let expectedMinutesInOven* = 40

let preparationMinutesPerLayer = 2 ## \
  ## The amount of minutes it takes to prepare a single layer.

proc remainingMinutesInOven*(actualMinutesInOven: int): int =
  ## Determine the amount of minutes the lasagna still needs to remain in the
  ## oven to be properly prepared.
  expectedMinutesInOven - actualMinutesInOven

proc preparationTimeInMinutes*(numberOfLayers: int): int =
  ## Given a number of layers, determine the total preparation time.
  numberOfLayers * preparationMinutesPerLayer

proc totalTimeInMinutes*(numberOfLayers, actualMinutesInOven: int): int =
  ## Calculate the total working time. That is, the time to prepare all the layers
  ## of lasagna, and the time already spent in the oven.
  preparationTimeInMinutes(numberOfLayers) + actualMinutesInOven
