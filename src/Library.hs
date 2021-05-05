module Library where
import PdePreludat


data Persona = Persona {
    edad :: Number,
    sueniosPorCumplir :: [Suenios],
    nombre :: String,
    felicidonios :: Number,
    habilidades :: [String]
} deriving (Show)


type Suenios = Persona -> Persona


fede :: Persona
fede = Persona {
    edad = 20,
    nombre = "fede",
    sueniosPorCumplir = [queTodoSigaIgual],
    felicidonios = 101,
    habilidades = ["nada jaja"]
} 


cantidadDeSuenios :: Persona -> Number
cantidadDeSuenios persona = (length.sueniosPorCumplir) persona

-- Punto 1

    -- Punto a
coeficienteSatisfaccion :: Persona -> Number
coeficienteSatisfaccion persona
    | ((>100).felicidonios) persona = (felicidonios persona * edad persona) 
    | ((((<= 100).felicidonios) persona)  && (((>50).felicidonios) persona)) = (cantidadDeSuenios persona) * felicidonios persona
    | otherwise = truncate (((/2).felicidonios) persona)


    -- Punto b
gradoDeAmbicion :: Persona -> Number
gradoDeAmbicion persona 
    |((>100).felicidonios) persona = felicidonios persona * cantidadDeSuenios persona
    |((((<= 100).felicidonios) persona)  && (((>50).felicidonios) persona)) = edad persona * (cantidadDeSuenios persona)
    |otherwise = ((*2).cantidadDeSuenios) persona



  -- Punto 2

    -- Punto a
nombreLargo :: Persona -> Bool
nombreLargo persona
    | ((>10).length.nombre) persona = True 
    | otherwise = False

    -- Punto b
personaSuertuda :: Persona -> Bool
personaSuertuda = elTripleEsPar.coeficienteSatisfaccion

elTripleEsPar :: Number -> Bool
elTripleEsPar = even.(*3)


-- Punto 3

recibirse :: String -> Suenios
recibirse carrera persona = persona {
    felicidonios = felicidonios persona + (((*1000) . length) carrera),
    habilidades = ((habilidades persona) ++ [carrera])}



viajar :: [String] -> Suenios
viajar [viajes] persona = persona {
    felicidonios = felicidonios persona + (((*100) . length) [viajes]),
    edad = ((+1).edad) persona}


queTodoSigaIgual :: Suenios
queTodoSigaIgual persona = persona {
    sueniosPorCumplir = []
}

comboPerfecto :: Suenios
comboPerfecto = (recibirse "Medicina" ) . (viajar ["Berazategui", "Paris"]) 
