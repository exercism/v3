module Booleans where

import Prelude

canExecuteFastAttack :: Boolean -> Boolean
canExecuteFastAttack knightIsAwake = not knightIsAwake

canSpy :: Boolean -> Boolean -> Boolean -> Boolean
canSpy knightIsAwake archerIsAwake prisonerIsAwake = knightIsAwake || archerIsAwake || prisonerIsAwake
