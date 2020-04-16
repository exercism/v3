module Test.Main where

import Prelude

import Booleans (canExecuteFastAttack, canSpy)
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
    it "Knight is asleep" $ canExecuteFastAttack false `shouldEqual` true
  describe "Can spy" do
      it "Prisoner is awake" $ canSpy false false true `shouldEqual` true
      it "Archer is awake" $ canSpy false true false `shouldEqual` true
      it "Knight is awake" $ canSpy true false false `shouldEqual` true
      it "Knight and prisoner are awake" $ canSpy true false true `shouldEqual` true
      it "Knight and archer are awake" $ canSpy true true false `shouldEqual` true
      it "Everyone is awake" $ canSpy true true true `shouldEqual` true
      it "Everyone is asleep" $ canSpy false false false `shouldEqual` false
