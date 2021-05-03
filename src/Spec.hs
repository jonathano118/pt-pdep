module Spec where
import PdePreludat
import Library
import Test.Hspec

correrTests :: IO ()
correrTests = hspec $ do
  describe "Test punto 1" $ do
    it "Coeficiente de satisfaccion de Fede" $ do
      coeficienteSatisfaccion fede `shouldBe` 3000

  describe "Test punto 2" $ do
    it "Nombre de Fede" $ do
      fede `shouldNotSatisfy` nombreLargo
