{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
module Ficha3 where

data Hora = H Int Int
    deriving (Show, Eq, Ord)

type Etapa = (Hora,Hora)
type Viagem = [Etapa]

vgm :: Viagem
vgm = [(H 9 30, H 10 25), (H 11 20, H 12 45), (H 13 30, H 14 45)]

-- 1)
-- a)
checkHour :: Hora -> Bool
checkHour (H h m)   | h >= 0 && h <= 24 && m >= 0 && m <= 60 = True
                    | otherwise = False

checkEtapa :: Etapa -> Bool
checkEtapa (h1,h2) = checkHour h1 && checkHour h2 && h2 > h1

-- b)
checkViagem :: Viagem -> Bool
checkViagem [] = True
checkViagem [e] = checkEtapa e
checkViagem ((h1,h2):(h3,h4):t)
    | checkEtapa (h1,h2) && checkEtapa (h3,h4) && h3 >= h2 && checkViagem t = True
    | otherwise = False

-- c)
horasPC :: Viagem -> Etapa
horasPC ((h1,h2):t) = (h1,(f(last t)))
    where f (h1,h2) = h2

-- Hour/Min To Min/Hour Converter
horaToMin :: Hora -> Int
horaToMin (H h m) = h * 60 + m

minToHora :: Int -> Hora
minToHora a = (H (div a 60) (mod a 60))

-- d)
tempoV :: Viagem -> Hora
tempoV [] = H 0 0
tempoV [(h1,h2)] = minToHora(horaToMin h2 - horaToMin h1) 
tempoV ((h1,h2):t) = minToHora((horaToMin h2 - horaToMin h1) + horaToMin (tempoV t))

-- e)
tempoE :: Viagem -> Hora
tempoE [] = H 0 0
tempoE [e] = H 0 0
tempoE (((_),(h1)):((h2),(h3)):t) = minToHora(horaToMin h2 - horaToMin h1)

-- f)
tempoTV :: Viagem -> Hora
tempoTV [] = H 0 0
tempoTV v = (H (div ((horaToMin horaF) - (horaToMin horaI)) 60) (mod ((horaToMin horaF) - (horaToMin horaI)) 60))
    where
        (horaI, horaF) = horasPC v

-- 2)
type Poligonal = [Ponto]
type Ponto = (Float,Float)
data Figura = Circulo Ponto Double
    | Rectangulo Ponto Ponto
    | Triangulo Ponto Ponto Ponto
        deriving (Show,Eq)
-- a)
comprimento :: Ponto -> Ponto -> Float
comprimento (x1,y1) (x2,y2) = sqrt((y2-y1)^2 + (x2-x1)^2)

-- b)
fechada :: Poligonal -> Bool
fechada [] = False
fechada [p] = False
fechada (h:t)   | fst h == snd (last t) = True 
                | otherwise = False

-- c)
plgn :: Poligonal
plgn = [(1,5),(3,4),(2,10),(10,3)]
triangulo :: Poligonal -> [Figura]
triangulo p | length p < 3 = []
triangulo (p1:p2:p3:t) | length (p1:p2:p3:t) > 2 = [(Triangulo p1 p2 m), (Triangulo p1 p2 m), (Triangulo p1 p3 m)] ++ triangulo (p2:p3:t)
    where m = middle (p1:p2:p3:t)

middle :: Poligonal -> Ponto
middle ((x1,y1):(x2,y2):(x3,y3):t) = ((x1+x2+x3)/3,(y1+y2+y3)/3)

-- d)
areaPLGN :: Poligonal -> Float
areaPLGN p | length p < 3 = 0
areaPLGN [(x1,y1),(x2,y2),(x3,y3)] = abs ((x1 * (y2 - y3) + x2 * (y3 - y1) + x3 * (y1 - y2)) / 2)
areaPLGN [p1, p2, p3, p4] = abs $ comprimento p1 p2 * comprimento p2 p3

-- e)



data Contacto = Casa Integer
    | Trab Integer
    | Tlm Integer
    | Email String
        deriving Show
type Nome = String
type Agenda = [(Nome, [Contacto])]