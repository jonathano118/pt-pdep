module Spec where
import PdePreludat
import Library
import Test.Hspec

-- Datos de prueba
fran :: Persona
fran = fran {
edad = 26,
nombre = "Francisco",
sueniosPorCumplir = [recibirse "Ingenieria"],
felicidonios = 40,
habilidades = ["acrobacia"]}

flor :: Persona
flor = flor {
edad = 29,
nombre = "Florencia",
sueniosPorCumplir = [recibirse "Doctora"],
felicidonios = 120,
habilidades = ["Pelear"]}

correrTests :: IO ()
correrTests = hspec $ do 

  -- Punto 1
  describe "Tests para validar si una persona está satisfecha con su vida" $ do 
    it "Coeficiente de satisfaccion de una persona muy feliz" $ do
      coeficienteSatisfaccion Persona {
      edad = 25,
      nombre = "Fede",
      sueniosPorCumplir = [recibirse "arquitectura", viajar ["Milan"]],
      felicidonios = 101,
      habilidades = ["Dibujar"]} `shouldBe` 2525

    it "Coeficiente de satisfaccion de una Persona moderadamente feliz" $ do
      coeficienteSatisfaccion Persona {
      edad = 25,
      nombre = "Fede",
      sueniosPorCumplir = [recibirse "diseño", viajar ["Caracas","Cancún"]],
      felicidonios = 100,
      habilidades = [""]} `shouldBe` 200  

    it "Coeficiente de satisfaccion de una Persona poco feliz" $ do
      coeficienteSatisfaccion Persona {
      edad = 25,
      nombre = "Fede",
      sueniosPorCumplir = [recibirse "arquitectura", viajar ["Caricuao"]],
      felicidonios = 50,
      habilidades = ["nada jaja"]} `shouldBe` 25


  describe "Tests para verificar la ambición de una persona" $ do
    it "Grado de ambición de una Persona muy feliz" $ do
      gradoDeAmbicion Persona{
      edad = 25,
      nombre = "Fede",
      sueniosPorCumplir = [recibirse "bióloga", viajar ["París","Roma"]],
      felicidonios = 101,
      habilidades = ["acrobacia"]} `shouldBe` 202

    it "Grado de ambición de una Persona moderadamente feliz" $ do
      gradoDeAmbicion Persona {
      edad = 26,
      nombre = "Fede",
      sueniosPorCumplir = [recibirse "bióloga", viajar ["París","Roma"]],
      felicidonios = 100,
      habilidades = ["acrobacia"]} `shouldBe` 52

    it "Grado de ambición de una Persona poco feliz" $ do
      gradoDeAmbicion Persona {
      edad = 26,
      nombre = "Fede",
      sueniosPorCumplir = [recibirse "bióloga"],
      felicidonios = 50,
      habilidades = ["acrobacia"]} `shouldBe` 2

-- Punto 2
  describe "Tests para verificar si tiene o no un nombre largo" $ do
    it "Una persona tiene un nombre largo" $ do
      Persona {
      edad = 20,
      nombre = "Maximiliano",
      sueniosPorCumplir = [recibirse "Docente"],
      felicidonios = 80,
      habilidades = ["Dormirse en el colectivo"]} `shouldSatisfy` nombreLargo

    it "Una persona no tiene un nombre largo" $ do
      Persona {
      edad = 32,
      nombre = "Evangelina",
      sueniosPorCumplir = [viajar ["Bariloche"]],
      felicidonios = 30,
      habilidades = ["Saltar en un pie"]} `shouldNotSatisfy` nombreLargo



  describe "Tests que validan si una persona es suertuda" $ do
    it "Una persona no es suertuda" $ do
      Persona {
      edad = 23,
      nombre = "johnny",
      sueniosPorCumplir = [recibirse "biólogo", viajar ["París","Roma"]],
      felicidonios = 14,
      habilidades = ["Nadar"]} `shouldNotSatisfy` personaSuertuda

    it "Una persona es suertuda" $ do
      Persona {
      edad = 22,
      nombre = "Carol",
      sueniosPorCumplir = [recibirse "bióloga", viajar ["París","Roma"]],
      felicidonios = 12,
      habilidades = ["acrobacia"]} `shouldSatisfy` personaSuertuda

 --Punto 3

  describe "Tests que cumplen el sueño de una persona de recibirse" $ do
    it "Felicidonios de una persona que se recibe" $ do
     (felicidonios.recibirse "Ingenieria") fran `shouldBe` 10040 

    it "Habilidades de una persona que se recibe" $ do
     (habilidades.recibirse "Ingenieria") fran `shouldBe` ["acrobacia","ingenieria"]


     
  describe "Tests que cumplen el sueño de una persona de viajar" $ do
    it "Felicidonios de una persona que viaja a dos ciudades" $ do
      (felicidonios.viajar ["París","Roma"]) flor `shouldBe` 320

    it "Edad de una persona que viaja" $ do
      (edad.viajar ["París","Roma"]) flor `shouldBe` 30



  describe "Tests que cumplen el sueño de una persona conformista" $ do
    it "Felicidonios de una persona quiere que todo siga igual" $ do
      (felicidonios.queTodoSigaIgual) fran `shouldBe` 40
    
    it "Edad de una persona quiere que todo siga igual" $ do
      (edad.queTodoSigaIgual) fran `shouldBe` 26
    
    it "Sueños de una persona quiere que todo siga igual" $ do
      (length.sueniosPorCumplir.queTodoSigaIgual) fran `shouldBe` 1

    it "Sueños de una persona quiere que todo siga igual" $ do
      (habilidades.queTodoSigaIgual) fran `shouldBe` ["acrobacias"]

  describe "Test que cumple el sueño de una persona de recibirse de Medicina y viajar a Berazategui y Paris" $ do
    it "Felicidonios de la persona" $ do
      (felicidonios.comboPerfecto) flor `shouldBe` 8420

    it "Felicidonios de la persona" $ do
      (edad.comboPerfecto) flor `shouldBe` 30

  describe "Tests que cumplen el sueño de una persona" $ do
    it "Felicidonios de la persona" $ do
      (habilidades.cumplirSuenios) fran `shouldBe` tail (habilidades fran)