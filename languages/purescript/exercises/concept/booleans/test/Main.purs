module Test.Main where

import Prelude

import Booleans
import Effect
import Effect.Aff
import Test.Spec
import Test.Spec.Assertions
import Test.Spec.Reporter.Console
import Test.Spec.Runner

main :: Effect Unit
main = launchAff_ $ runSpec [consoleReporter] do
  describe "test" do
    it "test" do
      number `shouldEqual` 3
