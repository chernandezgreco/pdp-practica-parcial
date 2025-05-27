module Library where
import PdePreludat
import Data.Char (toUpper)
-- import qualified Library as head


doble :: Number -> Number
doble numero = numero + numero

-- 1)
type Objeto = Barbaro -> Barbaro

data Barbaro = UnBarbaro {
  nombre :: String,
  fuerza :: Number,
  habilidades :: [String],
  objetos :: [Objeto]
} deriving (Show, Eq)



espada :: Number -> Objeto
espada peso barbaro = barbaro { fuerza = fuerza barbaro + 2 * peso}

amuletoMistico :: String -> Objeto
amuletoMistico = agregarHabilidad

varitaDefectuosa :: Objeto
varitaDefectuosa barbaro = agregarHabilidad "hacer Magia" barbaro { objetos = []}

ardilla :: Objeto
ardilla = id --funcion de haskell

cuerda :: Objeto -> Objeto -> Objeto
cuerda unObjeto otroObjeto = unObjeto . otroObjeto


-- 2)

megafono :: Objeto
megafono barbaro = barbaro {habilidades = potenciarHabilidades (habilidades barbaro) }


-- 3) 
type Evento = Barbaro -> Bool

invasionDeSuciosDuendes :: Evento
invasionDeSuciosDuendes = poseeHabilidad "Escribir Poesia Atroz"

cremalleraDelTiempo :: Evento
cremalleraDelTiempo barbaro = (||) (nombre barbaro == "Faffy") (nombre barbaro == "Astro")

ritualDeFechorias :: Evento
ritualDeFechorias barbaro = any (pasaPrueba barbaro) [saqueo, gritoDeGuerra, caligrafia]

pasaPrueba :: Barbaro -> Prueba -> Bool
pasaPrueba barbaro prueba = prueba barbaro

type Prueba = Barbaro -> Bool
saqueo :: Prueba
saqueo barbaro = (&&) (poseeHabilidad "Robar" barbaro) ((>80) . fuerza $ barbaro)

gritoDeGuerra :: Prueba
gritoDeGuerra barbaro = poderGrito barbaro > poderGritoNecesario barbaro
 
poderGrito :: Barbaro -> Number
poderGrito  = length . concat . habilidades

poderGritoNecesario :: Barbaro -> Number
poderGritoNecesario = (*4). length . objetos

caligrafia :: Prueba
caligrafia barbaro = masDeTresVocales barbaro && comienzaMayuscula barbaro

masDeTresVocales :: Barbaro -> Bool
masDeTresVocales barbaro = all tieneMasDeTresVocales (habilidades barbaro)

type Aventura = [Evento]

sobrevivientes :: [Barbaro] -> Aventura -> [Barbaro]
sobrevivientes barbaros aventura = filter (\ unBarbaro -> all (pasaPrueba unBarbaro) aventura) barbaros



-- 4) 

sinRepetidos :: Eq a => [a] -> [a]
sinRepetidos [] = []
sinRepetidos (x:xs)
  | elem x xs  = sinRepetidos xs
  | otherwise = x : sinRepetidos xs
  
-- iterate :: (a -> a) -> a -> [a]
  
descendiente :: Barbaro -> [Barbaro]
descendiente barbaro = iterate generarDescendiente barbaro


generarDescendiente :: Barbaro -> Barbaro 
generarDescendiente barbaro = aplicarObjetos (objetos barbaro) barbaro {habilidades = sinRepetidos (habilidades barbaro), nombre = nombre barbaro ++ "*"}

aplicarObjetos :: [Objeto] -> Barbaro -> Barbaro
aplicarObjetos unosObjetos hijoBarbaro = foldl aplicarUnObjeto hijoBarbaro unosObjetos

-------------------------------------------------------- AUXILIARES -----------------------------------------------------------------
agregarHabilidad :: String -> Barbaro -> Barbaro
agregarHabilidad nuevaHabilidad barbaro = barbaro {habilidades = nuevaHabilidad : habilidades barbaro}

potenciarHabilidades :: [String] -> [String]
potenciarHabilidades habilidades = [map toUpper (concat habilidades)]

poseeHabilidad :: String -> Barbaro -> Bool
poseeHabilidad habilidad barbaro = elem habilidad (habilidades barbaro)


tieneMasDeTresVocales :: String -> Bool
tieneMasDeTresVocales  = (>3).length.filter esVocal

comienzaMayuscula :: Barbaro -> Bool
comienzaMayuscula =  primeraLetraMayuscula.nombre

aplicarUnObjeto :: Barbaro -> Objeto -> Barbaro
aplicarUnObjeto unBarbaro unObjeto = unObjeto unBarbaro

primeraLetraMayuscula :: String -> Bool
primeraLetraMayuscula habilidades = True
-- primeraLetraMayuscula habilidades = head habilidad == toUpper . head habilidad

esVocal :: Char -> Bool
esVocal letra = elem letra "aeiouAEIOU"


dave :: Barbaro
dave = UnBarbaro {
  nombre = "belen",
  fuerza = 100,
  habilidades = [ "tejer", "escribir"],
  objetos = [ varitaDefectuosa, varitaDefectuosa, espada 3, espada 3] --espada para q sea un objeto le tengo que pasar parcialmente el param de number 
}

sinRepetidosNombre :: Eq a => [a] -> [a]
sinRepetidosNombre [] = []
sinRepetidosNombre (x:xs) -- lista de char entonces si
  | elem x xs  = sinRepetidosNombre xs
  | otherwise = x : sinRepetidosNombre xs
  
sinRepetidosObjetos :: Eq a => [a] -> [a]
sinRepetidosObjetos [] = [] -- no se puede porque no puedo comparar funciones 
sinRepetidosObjetos (x:xs)
  | elem x xs  = sinRepetidosObjetos xs
  | otherwise = x : sinRepetidosObjetos xs