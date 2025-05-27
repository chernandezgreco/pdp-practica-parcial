module Library where
import PdePreludat
import Data.Char (toUpper)
-- import qualified Library as head
doble :: Number -> Number
doble numero = numero + numero

type Peso = Number
type Tiempo = Number
type Grados = Number

data Gimnasta = UnGimnasta {
    peso :: Number,
    tonificacion :: Number
} deriving (Show,Eq)

data Rutina = UnaRutina {
    nombre :: String,
    duraconTotal :: Tiempo,
    ejercicios :: [Ejercicio]
}

-- 1) modelar gimnastas y operaciones necesarias para hacerlos ganar tonificacion y quemar calorias considerando que por cada 500 cal quemadas se baja 1 kg de peso 

tonificar :: Number -> (Gimnasta -> Gimnasta)
tonificar n gimnasta = gimnasta { tonificacion = tonificacion gimnasta + n}

quemarCalorias :: Number -> (Gimnasta -> Gimnasta)
quemarCalorias kcal gimnasta = gimnasta {peso = peso gimnasta - kcal `div` 500}


-- 2) --------------------------------------------------------------------------------------------

type Ejercicio = Tiempo -> Gimnasta -> Gimnasta
    -- En todas mis f se repitipo tiemp ogimnasta gimnasta
type Velocidad = Number

--a)

cinta :: Velocidad -> Ejercicio
cinta velocidad tiempo = quemarCalorias (tiempo * velocidad * 10)

caminata :: Ejercicio
caminata = cinta 5

pique :: Ejercicio
pique tiempoAsignado = cinta ((+20) tiempoAsignado) tiempoAsignado

--b)

pesas :: Peso -> Ejercicio
pesas peso tiempo
    | tiempo > 10 = tonificar peso
    | otherwise = id

-- c) 
colina :: Grados -> Ejercicio
colina inclinacion tiempo = quemarCalorias (2 * tiempo * inclinacion)

montania :: Grados -> Ejercicio
montania inclinacion tiempoAsignado  = tonificar 3 . colina (inclinacion + 5) tiempoAsignado  . colina inclinacion tiempoAsignado

-- 3) -----------------------------------------------------


realizarRutina :: Gimnasta -> Rutina -> Gimnasta
realizarRutina gimnastaInicial rutina = foldl (\gimnasta ejercicio -> ejercicio (tiempoParaEjercicio rutina) gimnasta ) gimnastaInicial (ejercicios rutina )


-- 4) 

mayorCantidadEjercicios :: [Rutina] -> Number
mayorCantidadEjercicios = maximum .map (length . ejercicios)

rutinaGanaTonificacion :: Gimnasta -> ([Rutina] -> [String])
rutinaGanaTonificacion gimnasta = map nombre . filter ((> tonificacion gimnasta) . tonificacion . realizarRutina gimnasta)

rutinaPeligrosa :: Gimnasta -> [Rutina] -> Bool
rutinaPeligrosa gimnasta = any ((< peso gimnasta `div` 2). peso . realizarRutina gimnasta)

---------------------------------------------------- AUXILIARES --------------------------------------------------

tiempoAsignado :: Number -> Tiempo
tiempoAsignado = (/2)

tiempoParaEjercicio :: Rutina -> Tiempo
tiempoParaEjercicio rutina = (div (duraconTotal rutina) . length .ejercicios) rutina