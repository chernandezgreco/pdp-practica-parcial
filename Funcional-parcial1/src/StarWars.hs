module Library where
import PdePreludat

-- 1 -----------------------------------------------------------------------------------------------------------------------

type PoderEspecial = Nave -> Nave

data Nave = UnaNave {
    nombre :: String,
    durabilidad :: Number,
    escudo :: Number,
    ataque :: Number,
    poder :: PoderEspecial
} deriving (Show, Eq)

tieFighter :: Nave
tieFighter = UnaNave {
    nombre = "TIE Fighter",
    durabilidad = 200,
    escudo = 100,
    ataque = 50,
    poder = turbo
}

xWing :: Nave
xWing = UnaNave {
    nombre = "X Wing",
    durabilidad = 300,
    escudo = 150,
    ataque = 100,
    poder = reparacionEmergencia
}

darthVader :: Nave
darthVader = UnaNave {
    nombre = "Darth Vader",
    durabilidad = 500,
    escudo = 300,
    ataque = 200,
    poder = superTrubo
}

millenniumFalcon :: Nave
millenniumFalcon = UnaNave {
    nombre = "Millennium Falcon",
    durabilidad = 1000,
    escudo = 500,
    ataque = 50,
    poder = reparacionEmergencia .incrementoEscudo
}

imperialDestructor :: Nave
imperialDestructor = UnaNave {
    nombre = "Imperial Destructor",
    durabilidad = 800,
    escudo = 80,
    ataque = 80,
    poder = destructorImperial
}

turbo :: PoderEspecial
turbo = modificarAtaque  (+25)

reparacionEmergencia :: PoderEspecial
reparacionEmergencia = modificarDurabilidad (+50) . modificarAtaque (+30)

superTrubo :: PoderEspecial
superTrubo = modificarAtaque ((*3) . (+50))  . modificarDurabilidad (\d -> d -45)

incrementoEscudo :: PoderEspecial
incrementoEscudo = modificarEscudo (+100)

destructorImperial :: PoderEspecial
destructorImperial = sacarPoderes . modificarEscudo (\e -> e -100)

-- 2 ---------------------------------------------------------------------------------------------------------------------------------------

durabilidadTotal :: [Nave] -> Number
durabilidadTotal = sum . map durabilidad

-- 3 ---------------------------------------------------------------------------------------------------------------------------------------
resultadoAtaque :: Nave -> Nave -> Nave
resultadoAtaque naveAtacante naveAtacada = siPuedeAtacar (activarPoder naveAtacante) (activarPoder naveAtacada)

siPuedeAtacar :: Nave -> Nave -> Nave
siPuedeAtacar naveAtacante naveAtacada = daniar( calcularAtaqueNave naveAtacante naveAtacada) naveAtacada

daniar :: Number -> Nave -> Nave
daniar c nave = nave { durabilidad = max 0 (durabilidad nave - c)}

calcularAtaqueNave :: Nave -> Nave -> Number
calcularAtaqueNave atacante atacada = max 0 (ataque atacante - escudo atacada)


activarPoder :: Nave -> Nave
activarPoder nave = (poder nave) nave --le pongo a la navve su propio pdoer

-- 4 ---------------------------------------------------------------------------------------------------------------------------------------
fueraDeBatalla :: Nave -> Bool
fueraDeBatalla = (==0) . durabilidad

-- 5 ---------------------------------------------------------------------------------------------------------------------------------------

type Estrategia = Nave -> Bool

navesDebiles :: Estrategia
navesDebiles = (<200) . escudo

navesCiertaPeligrosidad :: Number -> Estrategia
navesCiertaPeligrosidad peligrosidad = (>peligrosidad) . ataque

navesFueraDeCombate :: Nave -> Estrategia
navesFueraDeCombate naveAtacante = fueraDeBatalla . resultadoAtaque naveAtacante

-- yo tengo q tipo atacar ahora a las navs de la flota q CUMPLAN mi estrategia oseeaa filter ;)
-- no uso any como pense al princiopo porq me da un bool, no una lita

type Flota = [Nave]

misionSorpresa :: Estrategia -> Nave -> Flota -> Flota
misionSorpresa estrategia nave = convieneAtacar (siPuedeAtacar nave) estrategia

convieneAtacar :: (a -> a) -> (a -> Bool) -> [a] -> [a] -- esto me lo pso el vidual
convieneAtacar cambio condicion lista= map cambio (filter condicion lista ) ++ filter (not.condicion) lista
--- lo saque de la resoculon no entendia seria "aplicame el cambio a todos los de mi lita que cumplan mi condicion" "y concatenale los qno"

-- 6 ----------------------------------------------------------------------------------------------------------------------------------------

-- me dan 2 estrategia, starship y flota -> quiero esa estrategia q a la flota la deje con -durabilidad
-- misionSorpesa estrategia1 nave flota = me da cuanto queda la flota post taque con estrategia 1
-- durabilidadTotal (misionSorpresa estrategia1 nave flota) --> Cual es mi mj estrategia?

minimaDurabilidadSegunEstrategia :: Estrategia -> Estrategia -> Nave -> Flota -> Estrategia
minimaDurabilidadSegunEstrategia unaEstrategia otraEstrategia nave flota 
        | durabilidadTotal (misionSorpresa unaEstrategia nave flota) > durabilidadTotal (misionSorpresa otraEstrategia nave flota) = unaEstrategia
--       usando mi primera estrategia en mi misionSorpresa para la nave y flota la durabilidadTotal es menor q usar la segunda estrategia? si
        | otherwise = otraEstrategia

adelanteLaMision :: Estrategia -> Estrategia -> Nave -> Flota -> Flota
adelanteLaMision unaEstrategia otraEstrategia nave flota = misionSorpresa (minimaDurabilidadSegunEstrategia unaEstrategia otraEstrategia nave flota) nave flota
-- ahora lleva la MISION SORPESA usando la estrategia que menos durabiidad tiene (le paso todos los parametros q necesita) para esa nave y flota

-- 7 ------------------------------------------------------------------------------------------------------------------------------------------

flotaInfinita :: [Nave]
flotaInfinita = repeat imperialDestructor

-- no creo q se pueda determinar la durbilidad total porq si nunca termina, nunca va a poder parar de sumar y eso las durbiliades de cada ua








------------------------------------------------------------- AUXILIARES -------------------------------------------------------------------

modificarAtaque  ::( Number -> Number )-> Nave -> Nave
modificarAtaque  f nave = nave{ ataque = f (ataque nave)}

modificarDurabilidad :: (Number -> Number ) -> Nave -> Nave
modificarDurabilidad f nave = nave{ durabilidad = f (durabilidad nave)}

modificarEscudo ::(Number -> Number) -> Nave -> Nave
modificarEscudo f nave = nave{escudo = f (escudo nave)}

sacarPoderes :: Nave -> Nave  -- le doy la funcion id osea no haga NADA
sacarPoderes nave = nave{poder = id}