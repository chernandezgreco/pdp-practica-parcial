module Library where
import PdePreludat

data Aventurero = UnAventurero {
    nombre :: String,
    carga :: Number,
    salud :: Number,
    coraje :: Bool,
    criterioSeleccionEncuentros:: [Criterio]
} deriving (Show, Eq)

type Criterio = Aventurero -> Bool

--- 1) -------------------------------------------------------------------------------------------------

conformista :: Criterio
conformista _ = True

valiente :: Criterio
valiente aventurero = coraje aventurero || saludMas50 aventurero

tieneCoraje :: Aventurero -> Bool
tieneCoraje = coraje

saludMas50 :: Aventurero -> Bool
saludMas50 = (>50) . salud

lightPacker :: Number -> Criterio
lightPacker valor = (< valor ) . carga

aventurero1 :: Aventurero
aventurero1 = UnAventurero "gian" 100 60 True [conformista, lightPacker 5 ]

aventurero2 :: Aventurero
aventurero2 = UnAventurero "ale" 200 70 True [conformista, lightPacker 6 ]

aventurero3 :: Aventurero
aventurero3 = UnAventurero "jime" 300 60 True [conformista, lightPacker 7]

--2) ----------------------------------------------------------------------------------------------------

existeAventureroMas5Letras :: [Aventurero] -> Bool
existeAventureroMas5Letras = any mas5Letras

mas5Letras :: Aventurero -> Bool
mas5Letras = (>5) . length . nombre

sumaCarga :: [Aventurero] -> Number
sumaCarga = sum . map carga . filter cargaNumeroPar

cargaNumeroPar :: Aventurero -> Bool
cargaNumeroPar = even . carga

--3) -----------------------------------------------------------------------------------------------------

type EncuentroPersonaje = Aventurero -> Aventurero

esEncuentrosConPersonaje :: EncuentroPersonaje -> Aventurero -> Aventurero
esEncuentrosConPersonaje encuentro aventurero = descarga (encuentro aventurero)

descarga :: Aventurero -> Aventurero
descarga aventurero = aventurero {carga = carga aventurero -1}

curandero :: EncuentroPersonaje
curandero = modificarCarga (/2) . aumentarSalud (*0.20)

inspirador :: EncuentroPersonaje
inspirador = modificarCoraje True . aumentarSalud (*0.10)

embaucador :: EncuentroPersonaje 
embaucador = modificarCoraje False . modificarCarga (+10) .aumentarSalud (*(-0.5)) . convencerAventurer

cumpleConTodosCriterios :: Aventurero -> Bool
cumpleConTodosCriterios aventurero = all (\ criterio -> criterio aventurero ) (criterioSeleccionEncuentros aventurero)

encuentrosQueEnfrenta :: Aventurero -> [EncuentroPersonaje] -> [EncuentroPersonaje]
encuentrosQueEnfrenta aventurero [] = [] -- me da el aventurero y una lista vacia? entonces null list
encuentrosQueEnfrenta aventurero (x:xs) -- mi condicio              si el primero lo umple entonces el resto de la lista tambien
            | cumpleConTodosCriterios (esEncuentrosConPersonaje x aventurero) = x : encuentrosQueEnfrenta (esEncuentrosConPersonaje x aventurero) xs
            | otherwise = []
                


------------------------------------------------------------- AUXILIARES ------------------------------------------------------------------------

aumentarSalud :: (Number -> Number) -> Aventurero -> Aventurero
aumentarSalud porcentaje aventurero = aventurero { salud = salud aventurero + porcentaje (salud aventurero)}

modificarCarga :: (Number -> Number) -> Aventurero -> Aventurero
modificarCarga f aventurero = aventurero { carga = f (carga aventurero) }

modificarCoraje :: Bool -> Aventurero -> Aventurero
modificarCoraje nuevoValor aventurero = aventurero { coraje = nuevoValor }

convencerAventurer :: Aventurero -> Aventurero
convencerAventurer aventurero = aventurero{criterioSeleccionEncuentros = [lightPacker 10]}


factorial 0 = 1
factorial n = n * factorial (n-1)

f[]=0
f(x:xs)= x+f xs --sumo todos los num de la liasdta

takeHasta nto []=[]
takeHasta tope (x:xs)
        | x > tope = []
        | otherwise = x : takeHasta tope xs