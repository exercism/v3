module Booleans where

import Prelude

canExecuteFastAttack :: Boolean -> Boolean
canExecuteFastAttack knightIsAwake = not knightIsAwake

canSpy :: Boolean -> Boolean -> Boolean -> Boolean
canSpy knightIsAwake archerIsAwake prisonerIsAwake = knightIsAwake || archerIsAwake || prisonerIsAwake

canSignalPrisoner :: Boolean -> Boolean -> Boolean
canSignalPrisoner archerIsAwake prisonerIsAwake = not archerIsAwake && prisonerIsAwake

canFreePrisoner :: Boolean -> Boolean -> Boolean -> Boolean -> Boolean
canFreePrisoner knightIsAwake archerIsAwake prisonerIsAwake petDogIsPresent = not knightIsAwake && not archerIsAwake && prisonerIsAwake || petDogIsPresent && not archerIsAwake
