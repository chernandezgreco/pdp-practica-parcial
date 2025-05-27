module Library where
import PdePreludat

{--
1. a
En el universo local de Harry Postre, para hacer postres se utilizan hechizos que se van usando sobre los mismos para irlos preparando.
Modelar los postres. Un mismo postre puede tener muchos sabores, tiene un peso y se sirve a cierta temperatura.
Por ejemplo, un bizcocho borracho de fruta y crema de 100 gramos servido a 25°C.
--}

type Peso :: Number
type Temperatura :: Number
type Sabor :: String

data Postre = UnPostre{
    sabores :: [Sabor],
    peso :: Peso,
    temperatura :: Temperatura,
} deriving (Show, eq)

--un bizcocho borracho de fruta y crema de 100 gramos servido a 25°C.


{--
1.b
Modelar los hechizos, sabiendo que deberían poderse agregar más sin modificar el código existente. Por ahora existen los siguientes:
Incendio: calienta el postre 1 grado y lo hace perder 5% de su peso.
Immobulus: congela el postre, llevando su temperatura a 0.
Wingardium Leviosa: levanta el postre en el aire y lo deja caer, lo que agrega a sus sabores el sabor “concentrado”. Además, pierde 10% de su peso.
Diffindo: Corta el postre, disminuyendo su peso en el porcentaje indicado. 
Riddikulus: Requiere como información adicional un sabor y lo agrega a los sabores que tiene un postre, pero invertido.
Avada kedavra: Hace lo mismo que el immobulus pero además hace que el postre pierda todos sus sabores.

--}

type Hechizo :: Postre -> Postre

Incendio :: Hechizo
Incendio = perderPeso 5 . calienta 1

Immobulus :: Hechizo 
Immobulus postre = postre { temperatura = 0 }

WingardiumLeviosa :: Hechizo 
WingardiumLeviosa = perderPeso 100 . agregarSabor "concentrado"

Diffindo :: Hechizo
Diffindo = perderPeso 

Riddikulus :: Hechizo
Riddikulus sabor = agregarSabor (reverse sabor) 

AvadaKedavr :: Hechizo 
AvadaKedavr = immobulus . eliminarSabores 


--funciones a hacer luego
agregarSabor
calienta 
perderPeso