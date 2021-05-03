module Library where
import PdePreludat

doble :: Number -> Number
doble numero = numero + numero

data Persona = Persona {
    edad :: Number,
    sueniosPorCumplir :: [String],
    nombre :: String,
    felicidonios :: Number,
    habilidades :: [String]
} deriving (Show)


fede :: Persona
fede = Persona {
    edad = 20,
    sueniosPorCumplir = ["morir", "recibirme de prostituta"],
    nombre = "fede",
    felicidonios = 150,
    habilidades = ["nada jaja"]
} 


-- Punto 1

    -- Punto a
coeficienteSatisfaccion :: Persona -> Number
coeficienteSatisfaccion persona
    | ((>100).felicidonios) persona = (felicidonios persona * edad persona) 
    | (felicidonios persona <= 100 && felicidonios persona> 50) = (length.sueniosPorCumplir) persona * felicidonios persona
    | otherwise = truncate (((/2).felicidonios) persona)



  -- Punto 2

    -- Punto a
nombreLargo :: Persona -> Bool
nombreLargo persona
    | ((>10).length.nombre) persona = True 
    | otherwise = False