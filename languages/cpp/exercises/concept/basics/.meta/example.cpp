#include "basics.h"

int Lasagna::expectedMinutesInOven() { return 40; }

int Lasagna::remainingMinutesInOven(int actualMinutesInOven) {
  return expectedMinutesInOven() - actualMinutesInOven;
}

int Lasagna::preparationTimeInMinutes(int numberOfLayers) {
  return numberOfLayers * 2;
}

int Lasagna::elapsedTimeInMinutes(int numberOfLayers, int actualMinutesInOven) {
  return preparationTimeInMinutes(numberOfLayers) + actualMinutesInOven;
}
