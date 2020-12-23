module Cook exposing (elapsedTimeInMinutes, expectedMinutesInOven, preparationTimeInMinutes)


expectedMinutesInOven =
    40


preparationTimeInMinutes layers =
    2 * layers


elapsedTimeInMinutes layers passedAlready =
    passedAlready + preparationTimeInMinutes layers
