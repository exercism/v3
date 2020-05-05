module Test.Main where

import Prelude (Unit, discard, ($))

import Basics (expectedMinutesInOven, remainingMinutesInOven, preparationTimeInMinutes, totalTimeInMinutes)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)

main :: Effect Unit
main = launchAff_ $ runSpec [consoleReporter] do
  describe "Expected minutes in oven" do
    it "Expected minutes in oven" $ expectedMinutesInOven `shouldEqual` 40
  describe "Remaining minutes in oven" do
    it "After 25 minutes in oven" $ remainingMinutesInOven 25 `shouldEqual` 15
    it "After 35 minutes in oven" $ remainingMinutesInOven 35 `shouldEqual` 5
  describe "Preparation time in minutes" do
    it "4 layers of lasagna" $ preparationTimeInMinutes 4 `shouldEqual` 8
    it "2 layers of lasagna" $ preparationTimeInMinutes 2 `shouldEqual` 4
    it "456 layers of lasagna" $ preparationTimeInMinutes 456 `shouldEqual` 912
  describe "Total time in minutes" do
    it "1 layer, after 30 minutes" $ totalTimeInMinutes 1 30 `shouldEqual` 32
    it "4 layers, after 8 minutes" $ totalTimeInMinutes 4 8 `shouldEqual` 16
