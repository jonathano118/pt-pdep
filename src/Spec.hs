module Spec where
import PdePreludat
import Library
import Test.Hspec

correrTests :: IO ()
correrTests = hspec $ do
  describe "Tests para punto 1" $ do
    it "Coeficiente de satisfaccion de Fede" $ do
      coeficienteSatisfaccion fede{
      edad = 25,
      nombre = "Fede",
      sueniosPorCumplir = [recibirse "arquitectura", viajar ["Caricuao"]],
      felicidonios = 101,
      habilidades = ["nada jaja"]} `shouldBe` 2525

 
