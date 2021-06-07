module Library where
import PdePreludat


data Persona = Persona {
    edad :: Number,
    sueniosPorCumplir :: [Suenios],
    nombre :: String,
    felicidonios :: Number,
    habilidades :: [String]
} deriving (Eq, Show)


type Suenios = Persona -> Persona
type Fuente = Persona -> Persona


-- -- -- Punto 1

-- --     -- Punto a
-- -- coeficienteSatisfaccion :: Persona -> Number
-- -- coeficienteSatisfaccion persona
-- --     | ((>100).felicidonios) persona = felicidonios persona * edad persona
-- --     | ((>50).felicidonios) persona = cantidadDeSuenios persona * felicidonios persona
-- --     | otherwise = felicidonios persona `div` 2


-- --     -- Punto b
-- -- gradoDeAmbicion :: Persona -> Number
-- -- gradoDeAmbicion persona 
-- --     |((>100).felicidonios) persona = felicidonios persona * cantidadDeSuenios persona
-- --     |((>50).felicidonios) persona = edad persona * cantidadDeSuenios persona
-- --     |otherwise = cantidadDeSuenios persona * 2


-- -- cantidadDeSuenios :: Persona -> Number
-- -- cantidadDeSuenios = length.sueniosPorCumplir

-- --   -- Punto 2

-- --     -- Punto a
-- -- nombreLargo :: Persona -> Bool
-- -- nombreLargo = (>10).length.nombre

-- --     -- Punto b
-- -- personaSuertuda :: Persona -> Bool
-- -- personaSuertuda = elTripleEsPar.coeficienteSatisfaccion

-- -- elTripleEsPar :: Number -> Bool
-- -- elTripleEsPar = even.(*3)



-- Punto 3

sumarFelicidonios :: Number -> Suenios
sumarFelicidonios cantidad persona = persona {
    felicidonios =  felicidonios persona + cantidad
}

recibirse :: String -> Suenios
recibirse carrera persona = sumarFelicidonios 
                                      (1000 * length carrera) 
                                      persona { habilidades = ((habilidades persona) ++ [carrera]) }


viajar :: [String] -> Suenios
viajar [viajes] persona = sumarFelicidonios (100 * length [viajes]) 
                                            persona { edad = ((+1).edad) persona}


queTodoSigaIgual :: Suenios
queTodoSigaIgual = id


comboPerfecto :: Suenios
comboPerfecto = ((recibirse "Medicina" ) . (viajar ["Berazategui", "Paris"]) . (sumarFelicidonios 100))

-- Punto 4

    -- Punto a

fuenteMinimalista :: Fuente
fuenteMinimalista persona = ((head . sueniosPorCumplir) persona) persona {
    sueniosPorCumplir = (tail.sueniosPorCumplir) persona}



-- Punto 5

    -- Punto a

fuenteGanadora :: Ord a => [Fuente] -> Persona -> (Fuente -> Persona -> a) -> Fuente
fuenteGanadora [fuente] _ _ = fuente
fuenteGanadora (fuente:fuentes) persona criterio
    | criterio fuente persona > criterio (fuenteGanadora (fuentes) persona criterio) persona = fuente
    | otherwise = fuenteGanadora (fuentes) persona criterio

felicidoniosFuente :: Fuente -> Persona -> Number
felicidoniosFuente fuente persona = (felicidonios.fuente) persona 


