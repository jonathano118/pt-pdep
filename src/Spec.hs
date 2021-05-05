module Spec where
import PdePreludat
import Library
import Test.Hspec

correrTests :: IO ()
correrTests = hspec $ do 
  describe "Tests para punto 1a" $ do 
    it "Coeficiente de satisfaccion de una persona muy feliz" $ do
      coeficienteSatisfaccion fede{
      edad = 25,
      nombre = "Fede",
      sueniosPorCumplir = [recibirse "arquitectura", viajar ["Caricuao"]],
      felicidonios = 101,
      habilidades = ["nada jaja"]} `shouldBe` 2525

    it "Coeficiente de satisfaccion de una persona moderadamente feliz" $ do
      coeficienteSatisfaccion fede{
      edad = 25,
      nombre = "Fede",
      sueniosPorCumplir = [recibirse "arquitectura", viajar ["Caricuao","Cancún"]],
      felicidonios = 100,
      habilidades = ["nada jaja"]} `shouldBe` 200  

    it "Coeficiente de satisfaccion de una persona poco feliz" $ do
      coeficienteSatisfaccion fede{
      edad = 25,
      nombre = "Fede",
      sueniosPorCumplir = [recibirse "arquitectura", viajar ["Caricuao"]],
      felicidonios = 50,
      habilidades = ["nada jaja"]} `shouldBe` 25

  describe "Tests para punto 1b" $ do
    it "Grado de ambición de una persona muy feliz" $ do
      gradoDeAmbicion fede{
      edad = 25,
      nombre = "Fede",
      sueniosPorCumplir = [recibirse "bióloga", viajar ["París","Roma"]],
      felicidonios = 101,
      habilidades = ["acrobacia"]} `shouldBe` 202

    it "Grado de ambición de una persona moderadamente feliz" $ do
      gradoDeAmbicion fede{
      edad = 26,
      nombre = "Fede",
      sueniosPorCumplir = [recibirse "bióloga", viajar ["París","Roma"]],
      felicidonios = 100,
      habilidades = ["acrobacia"]} `shouldBe` 52

    it "Grado de ambición de una persona poco feliz" $ do
      gradoDeAmbicion fede{
      edad = 26,
      nombre = "Fede",
      sueniosPorCumplir = [recibirse "bióloga"],
      felicidonios = 50,
      habilidades = ["acrobacia"]} `shouldBe` 2

  describe "Tests para punto 2b" $ do
    it "Una persona no es suertuda" $ do
      fede{
      edad = 25,
      nombre = "Fede",
      sueniosPorCumplir = [recibirse "bióloga", viajar ["París","Roma"]],
      felicidonios = 14,
      habilidades = ["acrobacia"]} `shouldNotSatisfy` personaSuertuda

    it "Una persona es suertuda" $ do
      fede{
      edad = 26,
      nombre = "Fede",
      sueniosPorCumplir = [recibirse "bióloga", viajar ["París","Roma"]],
      felicidonios = 12,
      habilidades = ["acrobacia"]} `shouldSatisfy` personaSuertuda

 
