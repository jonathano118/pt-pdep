module Library where
import PdePreludat


data Persona = Persona {
    edad :: Number,
    sueniosPorCumplir :: [Suenios],
    nombre :: String,
    felicidonios :: Number,
    habilidades :: [String]
} deriving (Show)

type Suenios = Number
--type viajarACiudades :: Suenio
type Ciudades = String



{-data ViajarACiudades = ViajarACiudades {
    ciudades :: [Ciudades]
    cantidadDeCiudades :: CantidadDeCiudades
} deriving (Show)-



--CantidadDeCiudades :: ViajarACiudades -> Number
--cantidadDeCiudades [ciudades]  = (long.[ciudades]) 

visitaTodasLasCiudades :: Ciudades ->-}


fede :: Persona
fede = Persona {
    edad = 20,
    nombre = "fede",
    sueniosPorCumplir = ["recibirse", viajar],
    felicidonios = 150,
    habilidades = ["nada jaja"]
} 


-- Punto 1

    -- Punto a
coeficienteSatisfaccion :: Persona -> Number
coeficienteSatisfaccion persona
    | ((>100).felicidonios) persona = (felicidonios persona * edad persona) 
    | (felicidonios persona <= 100 && felicidonios persona> 50) = sueniosPorCumplir persona * felicidonios persona
    | otherwise = truncate (((/2).felicidonios) persona)

    -- Punto b
gradoDeAmbicion :: Persona -> Number
gradoDeAmbicion persona 
    |((>100).felicidonios) persona = felicidonios persona * cantidadDeSueños
    |(50 < felicidonios persona && felicidonios persona <= 100) = edad persona * cantidadDeSueños
    |otherwise = cantidadDeSueños * 2

cantidadDeSueños :: Persona -> Number
cantidadDeSueños [Suenios] persona = long.[Suenios] persona

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

    {-recibirse :: String -> Suenios
    recibirse carrera persona = persona {
        felicidonios = felicidonios + ((length.carrera) * 1000)}



    viajar :: [String] -> Suenios
    viajar [viajes] persona = persona {
        felicidonios = (*100) long.[viajes],
        edad = edad + 1
    }-}