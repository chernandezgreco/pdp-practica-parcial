module Library where
import PdePreludat

data Mago = UnMago {
    nombre :: String,
    edad :: Number,
    salud :: Number,
    hechizos :: [Hechizo]
} deriving (Show, Eq)

type Hechizo = Mago -> Mago

-- 1) -----------------------------------------------------------------------------------------

-- 1a)

curar :: Number -> Hechizo
curar = alterarVida

-- 1B)

lanzarRayo :: Hechizo
lanzarRayo mago
        | salud mago >10  = alterarVida (-10) mago
        | otherwise       = alterarVida (sacarVida mago) mago

-- 1C) 

amnesia :: Number -> Hechizo
amnesia = olvidarNHechizos

olvidarNHechizos :: Number -> Mago -> Mago
olvidarNHechizos n mago = mago{ hechizos = drop n (hechizos mago)}

-- 1D)

confundir :: Hechizo
confundir mago = primerHechizo mago mago



-- 2) --------------------------------------------------------------------------------------------------------------------------------------

-- 2a) 

poder :: Mago -> Number
poder mago = salud mago - edadXhechizos mago

-- 2b)

danio :: Hechizo -> Mago -> Number
danio hechizo mago = salud mago - (salud . hechizo) mago

-- 2c)

diferenciaDePoder :: Mago -> Mago -> Number
diferenciaDePoder unMago otroMago = abs (poder unMago - poder otroMago)

-- 3) ---------------------------------------------------------------------------------------------------------------------------------------

data Academia = Academia {
    magos :: [Mago],
    examenDeIngreso :: Mago -> Bool
}

-- 3a)

estaRincewind :: Academia -> Bool
estaRincewind academia = any esRincenwind (magos academia) -- hay alguno que cumpla mi funcion de los magos de la academia

esRincenwind :: Mago -> Bool
esRincenwind mago = nombre mago == "Rincenwind" && (null . hechizos) mago -- lista vacia null

-- 3b)

todosSonNionios :: Academia -> Bool
todosSonNionios academia = all esNionio (magos academia)

esNionio :: Mago -> Bool
esNionio mago = esViejo mago && masHechizosQsalud mago

masHechizosQsalud :: Mago -> Bool
masHechizosQsalud mago = cantidadHechizos mago > salud mago

esViejo :: Mago -> Bool
esViejo mago = salud mago > 50 

-- 3c)

cantidadNoPasaronExamen :: Academia -> Number
cantidadNoPasaronExamen = length . quienesNoPasaronExamen  -- CUANTOS no pasaron osea cuanto mi lista de los q filtre

quienesNoPasaronExamen :: Academia -> [Mago]
quienesNoPasaronExamen academia = filter (noPasaronExamen academia) (magos academia)  -- filtrame quienes no pasaron y dame la listade esos magos

noPasaronExamen :: Academia -> Mago -> Bool
noPasaronExamen academia = not. examenDeIngreso academia -- no pasaron examen ingreso academia 

-- 3d)

sumatoriaEdad :: Academia -> Number
sumatoriaEdad = sum . map edad . filter tienenMas10Hechizos . magos

tienenMas10Hechizos :: Mago -> Bool
tienenMas10Hechizos = (>10) . cantidadHechizos


-- 5)

hechizarlo :: [Hechizo] -> Mago -> Mago
hechizarlo hechizos mago = foldl (flip ($)) mago hechizos -- aplico 1 a 1 los hechizos sobre el mago

noPuedeGanarle :: Mago -> Mago -> Bool
noPuedeGanarle unMago otroMago = salud unMago == salud (hechizarlo (hechizos otroMago) unMago) 

------------------------------------------------------------------- AUXILIARES --------------------------------------------------------------

alterarVida :: Number -> Mago -> Mago
alterarVida numero mago = mago {salud = salud mago + numero}

sacarVida :: Mago -> Number
sacarVida = (/(-2)) . salud

primerHechizo :: Mago -> Hechizo
primerHechizo = head . hechizos

edadXhechizos :: Mago -> Number
edadXhechizos mago = edad mago * cantidadHechizos mago

cantidadHechizos :: Mago -> Number
cantidadHechizos = length . hechizos