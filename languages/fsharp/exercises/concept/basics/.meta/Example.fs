module Basics

let expectedMinutesInOven = 40

let remainingMinutesInOven actualMinutesInOven = expectedMinutesInOven - actualMinutesInOven

let preparationTimeInMinutes numberOfLayers = numberOfLayers * 2

let totalTimeInMinutes numberOfLayers actualMinutesInOven =
    preparationTimeInMinutes numberOfLayers + actualMinutesInOven
