module Basics where

import Prelude

expectedMinutesInOven :: Int
expectedMinutesInOven = 40

remainingMinutesInOven :: Int -> Int
remainingMinutesInOven minutesInOven = 40 - minutesInOven

preparationTimeInMinutes :: Int -> Int
preparationTimeInMinutes numberOfLayers = 2 * numberOfLayers

totalTimeInMinutes :: Int -> Int -> Int
totalTimeInMinutes numberOfLayers minutesInOven = minutesInOven + preparationTimeInMinutes numberOfLayers
