module Library where
import PdePreludat

doble :: Number -> Number
doble numero = numero + numero

data Postre = UnPostre{
    sabores :: [Sabor],
    peso :: Number,
    temperatura :: Number
}deriving (Show, Eq)

bizcocho = UnPostre ["Borracho", "Fruta", "Crema"] 100 25

type Sabor = String

--b

type Hechizo = Postre -> Postre

incendio :: Hechizo
incendio = perderTemperatura (-1) . perderPeso 5

perderTemperatura :: Number ->Postre -> Postre
perderTemperatura n postre = postre{temperatura = temperatura postre - n}

perderPeso :: Number -> Postre -> Postre
perderPeso n postre = postre{peso = peso postre * (100-n)/100}

agregarSabor :: Sabor -> Postre -> Postre
agregarSabor sabor postre = postre{sabores = sabor:sabores postre}

immobulus :: Hechizo
immobulus postre = perderTemperatura (temperatura postre) postre

wingardiumLeviosa :: Hechizo
wingardiumLeviosa = agregarSabor "concentrado" . perderPeso 10

diffindo :: Number -> Hechizo
diffindo = perderPeso

riddikulus :: Sabor -> Hechizo
riddikulus sabor  = agregarSabor (reverse sabor)

avadaKedavra :: Hechizo
avadaKedavra = immobulus . perderSabores

perderSabores :: Postre -> Postre
perderSabores postre = postre{sabores = []}

--c

quedanListos :: Hechizo -> [Postre] -> Bool
quedanListos hechizo = all (estaListo . hechizo)

estaListo :: Postre -> Bool
estaListo postre = pesaMasQueCero postre && tieneAlgunSabor postre && noEstaCongelado postre

pesaMasQueCero :: Postre -> Bool
pesaMasQueCero  = (>0) . peso  

tieneAlgunSabor :: Postre -> Bool
tieneAlgunSabor = (>0).length . sabores  

noEstaCongelado :: Postre -> Bool
noEstaCongelado = (>0).temperatura

--No hace falta, otra forma
--aplicarHechizo :: Hechizo -> [Postre] -> [Postre]
--aplicarHechizo = map 

--D

postresListos :: [Postre] -> [Postre]
postresListos = filter estaListo

pesoPromedio :: [Postre] -> Number
pesoPromedio = promedio . map peso . postresListos

promedio ::[Number] -> Number
promedio numeros = sum numeros / length numeros

--2


data Mago = UnMago{
    hechizos :: [Hechizo],
    cantHorrorcruxes :: Number
}deriving(Show, Eq)



practicar :: Hechizo -> Postre -> Mago -> Mago
practicar hechizo postre = usarHechizo hechizo postre . agregarHechizo hechizo 

agregarHechizo :: Hechizo -> Mago -> Mago
agregarHechizo hechizo mago = mago{hechizos = hechizo:hechizos mago}

usarHechizo :: Hechizo -> Postre -> Mago -> Mago
usarHechizo hechizo postre mago
    |hechizo postre == avadaKedavra postre = mago{cantHorrorcruxes = cantHorrorcruxes mago + 1}
    |otherwise = mago

--B

obtenerMejorHechizo :: Postre -> Mago -> Hechizo
obtenerMejorHechizo postre mago = elMejor postre (hechizos mago)   

elMejor :: Postre -> [Hechizo] -> Hechizo
elMejor postre [hechizo] = hechizo
elMejor postre (h1:h2:restoHechizos) 
    |esMejor postre h1 h2 = elMejor postre (h1:restoHechizos)
    |otherwise = elMejor postre (h2:restoHechizos)

esMejor :: Postre -> Hechizo -> Hechizo -> Bool
esMejor postre hechizo1 hechizo2 = cantSabores hechizo1 postre > cantSabores hechizo2 postre

cantSabores :: Hechizo -> Postre -> Number
cantSabores hechizo = length . sabores . hechizo

--3

mesaInfinita :: [Postre]
mesaInfinita = repeat bizcocho

magoConHechizosInfinitos :: Mago
magoConHechizosInfinitos = UnMago (repeat avadaKedavra) 2

--pruebas
harry :: Mago
harry = UnMago [immobulus, avadaKedavra, diffindo 1] 1

primerHechizo :: Mago -> Hechizo
primerHechizo mago = head (hechizos mago)

--b 
--verdadero, avadaKedavra por ej ya que no es necesario que recorra toda la lista para dar la respuesta solo con q el primero no cumpla devuelve false

--No ya que se debe recorrer la lista infinita para determinar el mejor hechizo, aun con lazy evaluation