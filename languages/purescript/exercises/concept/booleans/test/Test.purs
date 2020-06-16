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
    it "Knight is asleep" $ canExecuteFastAttack false `shouldEqual` true
  describe "Can spy" do
    it "Knight is asleep, archer is asleep, prisoner is awake" $ canSpy false false true `shouldEqual` true
    it "Knight is asleep, archer is awake, prisoner is asleep" $ canSpy false true false `shouldEqual` true
    it "Knight is awake, archer is asleep, prisoner is asleep" $ canSpy true false false `shouldEqual` true
    it "Knight is awake, archer is asleep, prisoner is awake" $ canSpy true false true `shouldEqual` true
    it "Knight is awake, archer is awake, prisoner is asleep" $ canSpy true true false `shouldEqual` true
    it "Knight is awake, archer is awake, prisoner is awake" $ canSpy true true true `shouldEqual` true
    it "Knight is asleep, archer is asleep, prisoner is asleep" $ canSpy false false false `shouldEqual` false
  describe "Can signal prisoner" do
    it "Archer is asleep, prisoner is asleep" $ canSignalPrisoner false false `shouldEqual` false
    it "Archer is asleep, prisoner is awake" $ canSignalPrisoner false true `shouldEqual` true
    it "Archer is awake, prisoner is asleep" $ canSignalPrisoner true false `shouldEqual` false
    it "Archer is awake, prisoner is awake" $ canSignalPrisoner true true `shouldEqual` false
  describe "Can free prisoner" do
    it "Knight is asleep, archer is asleep, prisoner is asleep, pet dog is not present" $ canFreePrisoner false false false false `shouldEqual` false
    it "Knight is asleep, archer is asleep, prisoner is asleep, pet dog is present" $ canFreePrisoner false false false true `shouldEqual` true
    it "Knight is asleep, archer is asleep, prisoner is awake, pet dog is not present" $ canFreePrisoner false false true false `shouldEqual` true
    it "Knight is asleep, archer is asleep, prisoner is awake, pet dog is present" $ canFreePrisoner false false true true `shouldEqual` true
    it "Knight is asleep, archer is awake, prisoner is asleep, pet dog is not present" $ canFreePrisoner false true false false `shouldEqual` false
    it "Knight is asleep, archer is awake, prisoner is asleep, pet dog is present" $ canFreePrisoner false true false true `shouldEqual` false
    it "Knight is asleep, archer is awake, prisoner is awake, pet dog is not present" $ canFreePrisoner false true true false `shouldEqual` false
    it "Knight is asleep, archer is awake, prisoner is awake, pet dog is present" $ canFreePrisoner false true true true `shouldEqual` false
    it "Knight is awake, archer is asleep, prisoner is asleep, pet dog is not present" $ canFreePrisoner true false false false `shouldEqual` false
    it "Knight is awake, archer is asleep, prisoner is asleep, pet dog is present" $ canFreePrisoner true false false true `shouldEqual` true
    it "Knight is awake, archer is asleep, prisoner is awake, pet dog is not present" $ canFreePrisoner true false true false `shouldEqual` false
    it "Knight is awake, archer is asleep, prisoner is awake, pet dog is present" $ canFreePrisoner true false true true `shouldEqual` true
    it "Knight is awake, archer is awake, prisoner is asleep, pet dog is not present" $ canFreePrisoner true true false false `shouldEqual` false
    it "Knight is awake, archer is awake, prisoner is asleep, pet dog is present" $ canFreePrisoner true true false true `shouldEqual` false
    it "Knight is awake, archer is awake, prisoner is awake, pet dog is not present" $ canFreePrisoner true true true false `shouldEqual` false
    it "Knight is awake, archer is awake, prisoner is awake, pet dog is present" $ canFreePrisoner true true true true `shouldEqual` false
