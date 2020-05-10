module Test.Main where

import Prelude

import PatternMatching (reply)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)

main :: Effect Unit
main = launchAff_ $ runSpec [consoleReporter] do
  describe "Correct" do
    it "Give hint for 42" $ reply 42 `shouldEqual` "Correct"
  describe "So close" do
    it "Give hint for 41" $ reply 41 `shouldEqual` "So close"
    it "Give hint for 43" $ reply 43 `shouldEqual` "So close"
  describe "Too low" do
    it "Give hint for 40" $ reply 40 `shouldEqual` "Too low"
    it "Give hint for 1" $ reply 1 `shouldEqual` "Too low"
  describe "Too high" do
    it "Give hint for 44" $ reply 44 `shouldEqual` "Too high"
    it "Give hint for 100" $ reply 100 `shouldEqual` "Too high"
