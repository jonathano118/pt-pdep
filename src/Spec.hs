module Spec where
import PdePreludat
import Library
import Test.Hspec

correrTests :: IO ()
correrTests = hspec $ do
  describe "Tests para punto 1" $ do
    it "Coeficiente de satisfaccion de Fede" $ do
      coeficienteSatisfaccion {
      edad = 25,
      nombre = "Fede",
      sueniosPorCumplir = 5,
      felicidonios = 101,
      habilidades = ["nada jaja"]} `shouldBe` 2525

    it "Coeficiente de satisfaccion de Cami" $ do
      coeficienteSatisfaccion persona {
      edad = 22,
      nombre = "Cami",
      sueniosPorCumplir = 2,
      felicidonios = 100,
      habilidades = ["nada jaja"]} `shouldBe` 200

