module Test.Main where

import Prelude

import Booleans (canExecuteFastAttack, canSpy, canSignalPrisoner, canFreePrisoner)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)

main :: Effect Unit
main = launchAff_ $ runSpec [consoleReporter] do
  describe "Can fast attack" do
    it "Knight is awake" $ canExecuteFastAttack true `shouldEqual` false
