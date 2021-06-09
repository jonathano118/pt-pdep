module Spec where
import PdePreludat
import Library
import Test.Hspec

-- Datos de prueba
fran :: Persona
fran = Persona {
      edad = 26,
      nombre = "Francisco",
      sueniosPorCumplir = [viajar["Ituzaingo"]],
      felicidonios = 40,
      habilidades = ["Dormirse temprano"]}

flor :: Persona
flor = Persona {
      edad = 29,
      nombre = "Florencia",
      sueniosPorCumplir = [recibirse "Doctora", queTodoSigaIgual, viajar ["Caracas", "La Plata"]],
      felicidonios = 90,
      habilidades = ["Pelear"]}

joni::Persona
joni = Persona {
      edad = 23,
      nombre = "Jonathan",
      sueniosPorCumplir = [recibirse "Ingeiniero", recibirse "Programador"],
      felicidonios = 80,
      habilidades = ["Nada"]}

-- fio :: Persona
-- fio = Persona {
--       edad = 25,
--       nombre = "Fiona",
--       sueniosPorCumplir = [repeat queTodoSigaIgual],
--       felicidonios = 80,
--       habilidades = ["Nada"]
-- }

correrTests :: IO ()
correrTests = hspec $ do 

--   -- Punto 1
--   describe "Tests para validar si una persona está satisfecha con su vida" $ do 
--     it "Coeficiente de satisfaccion de una persona muy feliz" $ do
--       coeficienteSatisfaccion flor`shouldBe` 2525

--     it "Coeficiente de satisfaccion de una Persona moderadamente feliz" $ do
--       coeficienteSatisfaccion flor `shouldBe` 200  

--     it "Coeficiente de satisfaccion de una Persona poco feliz" $ do
--       coeficienteSatisfaccion fran `shouldBe` 25


--   describe "Tests para verificar la ambición de una persona" $ do
--     it "Grado de ambición de una Persona muy feliz" $ do
--       gradoDeAmbicion flor `shouldBe` 202

--     it "Grado de ambición de una Persona moderadamente feliz" $ do
--       gradoDeAmbicion joni `shouldBe` 52

--     it "Grado de ambición de una Persona poco feliz" $ do
--       gradoDeAmbicion fran `shouldBe` 2

-- -- Punto 2
--   describe "Tests para verificar si tiene o no un nombre largo" $ do
--     it "Una persona tiene un nombre largo" $ do
--       maxi `shouldSatisfy` nombreLargo

--     it "Una persona no tiene un nombre largo" $ do
--       flor `shouldNotSatisfy` nombreLargo


--   describe "Tests que validan si una persona es suertuda" $ do
--     it "Una persona no es suertuda" $ do
--       joni `shouldNotSatisfy` personaSuertuda

--     it "Una persona es suertuda" $ do
--       fran `shouldSatisfy` personaSuertuda

--  --Punto 3

--   describe "Tests que cumplen el sueño de una persona de recibirse" $ do
--     it "Felicidonios de una persona que se recibe" $ do
--      (felicidonios.recibirse "Ingenieria") fran `shouldBe` 10040 

--     it "Habilidades de una persona que se recibe" $ do
--      (habilidades.recibirse "Ingenieria") fran `shouldBe` ["acrobacia","ingenieria"]

     
--   describe "Tests que cumplen el sueño de una persona de viajar" $ do
--     it "Felicidonios de una persona que viaja a dos ciudades" $ do
--       (felicidonios.viajar ["París","Roma"]) flor `shouldBe` 320

--     it "Edad de una persona que viaja" $ do
--       (edad.viajar ["París","Roma"]) flor `shouldBe` 30


--   describe "Tests que cumplen el sueño de una persona conformista" $ do
--     it "Felicidonios de una persona quiere que todo siga igual" $ do
--       (felicidonios.queTodoSigaIgual) fran `shouldBe` 40
    
--     it "Edad de una persona quiere que todo siga igual" $ do
--       (edad.queTodoSigaIgual) fran `shouldBe` 26
    
--     it "Sueños de una persona quiere que todo siga igual" $ do
--       (length.sueniosPorCumplir.queTodoSigaIgual) fran `shouldBe` 1

--     it "Sueños de una persona quiere que todo siga igual" $ do
--       (habilidades.queTodoSigaIgual) fran `shouldBe` ["acrobacias"]

--   describe "Test que cumple el sueño de una persona de recibirse de Medicina y viajar a Berazategui y Paris" $ do
--     it "Felicidonios de la persona" $ do
--       (felicidonios.comboPerfecto) flor `shouldBe` 8390

--     it "Edad de la persona" $ do
--       (edad.comboPerfecto) flor `shouldBe` 30




-- Entrega 2
      
      --Punto 4
      -- Punto a
  describe "Tests de fuente que cumple el primer sueño de una persona" $ do
    it "Felicidonios de la persona" $ do
     (felicidonios.fuenteCopada) joni `shouldBe` 21080

     
    it "Habilidades de la persona" $ do
     (habilidades.fuenteMinimalista) joni `shouldBe` ["Nada","Ingeiniero"]

    it "Sueños de la persona" $ do
     (length.sueniosPorCumplir.fuenteMinimalista) joni `shouldBe` 1


      --Punto b
  describe "Tests de fuente que le cumple todos los sueños a una persona" $ do
    it "Felicidonios de la persona" $ do
     (felicidonios.fuenteMinimalista) flor `shouldBe` 7090

    it "Habilidades de la persona" $ do
     (habilidades.fuenteCopada) flor `shouldBe` ["Pelear", "Doctora"]

    it "Sueños de la persona" $ do
     (length.sueniosPorCumplir.fuenteCopada) flor `shouldBe` 0
    

      -- Punto d
  describe "Tests de fuente que no cumple ningun deseo" $ do
    it "Felicidonios de la persona" $ do
     (felicidonios.fuenteSorda) fran `shouldBe` 40
     
    it "Habilidades de la persona" $ do
     (habilidades.fuenteSorda) fran `shouldBe` ["Dormirse temprano"]

    it "Sueños de la persona" $ do
     (length.sueniosPorCumplir.fuenteSorda) fran `shouldBe` 1


      --Punto 6

  describe "Test que verifica cuantos sueños de una persona son valiosos" $ do
    it "Cantidad de sueños valiosos" $ do
     (length.suenioValioso) flor `shouldBe` 2


                                                