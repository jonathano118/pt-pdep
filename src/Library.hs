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
viajar viajes persona = sumarFelicidonios (100 * length viajes) 
                                            persona { edad = ((+1).edad) persona}


queTodoSigaIgual :: Suenios
queTodoSigaIgual = id


comboPerfecto :: Suenios
comboPerfecto = ((recibirse "Medicina" ) . (viajar ["Berazategui", "Paris"]) . (sumarFelicidonios 100))

-- Punto 4

    -- Punto a

cumplirPrimerSuenio :: Persona -> Persona
cumplirPrimerSuenio persona = (head . sueniosPorCumplir) persona $ persona

quitarElPrimerSuenio :: Persona -> Persona 
quitarElPrimerSuenio persona = persona { sueniosPorCumplir = (tail.sueniosPorCumplir) persona}

fuenteMinimalista :: Fuente
fuenteMinimalista  = quitarElPrimerSuenio.cumplirPrimerSuenio

    -- Punto b

fuenteCopada :: Fuente
fuenteCopada persona = (foldr ($) persona (sueniosPorCumplir persona)) {
    sueniosPorCumplir = []}

    --Punto d
fuenteSorda :: Fuente
fuenteSorda = id



-- Punto 5

fuenteGanadora :: ([Fuente] -> Persona -> Fuente) -> [Fuente] -> Persona -> Fuente
fuenteGanadora = id

    -- Punto a
masFelicidonios :: [Fuente] -> Persona -> Fuente
masFelicidonios [fuente] _ = fuente
masFelicidonios (fuente:fuentes) persona
    | (felicidonios.fuente) persona > (felicidonios.(masFelicidonios (fuentes) persona)) persona = fuente
    | otherwise = masFelicidonios (fuentes) persona

    --Punto b
menosFelicidonios :: [Fuente] -> Persona -> Fuente
menosFelicidonios [fuente] _ = fuente
menosFelicidonios (fuente:fuentes) persona
    | (felicidonios.fuente) persona < (felicidonios.(menosFelicidonios (fuentes) persona)) persona = fuente
    | otherwise = menosFelicidonios (fuentes) persona

laFuenteGanadora:: Ord a => (Persona -> a) -> Persona ->[Fuente] ->Fuente
laFuenteGanadora _ _ [fuente] = fuente 
laFuenteGanadora criterio persona (primerFuente:segundaFuente:fuentes)
 | (criterio.primerFuente) persona >= (criterio.segundaFuente) persona = laFuenteGanadora criterio persona (primerFuente:fuentes)
 | otherwise = laFuenteGanadora criterio persona (segundaFuente:fuentes)

-- laFuenteGanadora (negate.felicidonios) pepe [fuenteCopada, fuenteMinimalista]


-- Punto 6

    --Punto a
suenioValioso :: Persona -> [Suenios]
suenioValioso persona =  filter ((>100).felicidonios.($ persona)) (sueniosPorCumplir persona)

    --Punto b
suenioRaro :: Persona -> Bool
suenioRaro persona = any ((==(felicidonios persona)).felicidonios.($ persona)) (sueniosPorCumplir persona)