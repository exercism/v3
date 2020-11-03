module Basics

let expectedMinutesInOven = 40

let remainingMinutesInOven actualMinutesInOven =
    expectedMinutesInOven - actualMinutesInOven

let preparationTimeInMinutes numberOfLayers = numberOfLayers * 2

let elapsedTimeInMinutes numberOfLayers actualMinutesInOven =
    preparationTimeInMinutes numberOfLayers
    + actualMinutesInOven
