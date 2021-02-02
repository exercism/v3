#if !defined(BASICS_H_)
#define BASICS_H_

class Lasagna {
public:
  int expectedMinutesInOven();
  int remainingMinutesInOven(int actualMinutesInOven);
  int preparationTimeInMinutes(int numberOfLayers);
  int elapsedTimeInMinutes(int numberOfLayers, int actualMinutesInOven);
};

#endif // !BASICS_H_
