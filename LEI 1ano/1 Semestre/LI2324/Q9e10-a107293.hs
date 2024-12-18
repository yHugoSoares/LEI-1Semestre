module Questao9e10 where

import Test.HUnit

type Coordenada = (Float,Float)
type Comprimento = Float
type Largura = Float
data Movimento = E | W | N | S  deriving (Show, Eq)
type Mapa = (Comprimento,Largura)


-- Questao 9
sequenciaMovimentos :: Mapa -> Coordenada -> [Movimento]
sequenciaMovimentos (comprimento, largura) (x, y)
  | x < comprimento / 2 && y < largura / 2 = replicate (floor x) W ++ replicate (floor y) S
  | x >= comprimento / 2 && y < largura / 2 = replicate (floor (comprimento - x)) E ++ replicate (floor y) S
  | x < comprimento / 2 && y >= largura / 2 = replicate (floor x) W ++ replicate (floor (largura - y)) N
  | otherwise = replicate (floor (comprimento - x)) E ++ replicate (floor (largura - y)) N


-- Questao 10
type Vidas = Int
data Personagem = Personagem Coordenada Vidas deriving (Show, Eq)

juncaoMapas :: [Personagem] -> [Personagem] -> [Personagem]
juncaoMapas mapa1 mapa2 = mapa1 ++ filter (\p2 -> not (elem p2 mapa1)) mapa2


-- Tests
testSequenciaMovimentos1 = test ["Personagem em canto inferior esquerdo" ~: [W, W, S, S, S] ~?= sequenciaMovimentos (10, 8) (2, 3)]
testSequenciaMovimentos2 = test ["Personagem em canto superior direito" ~: [E, E, N, N, N, N, N, N] ~?= sequenciaMovimentos (10, 8) (8, 5)]

testJuncaoMapas1 = test ["Junção de mapas com personagens diferentes" ~: 
    [Personagem (1, 2) 3, Personagem (4, 5) 6, Personagem (7, 8) 9, Personagem (10, 11) 12]
    ~=? juncaoMapas [Personagem (1, 2) 3, Personagem (4, 5) 6] [Personagem (7, 8) 9, Personagem (10, 11) 12]]

testJuncaoMapas2 = test["Junção de mapas com personagens sobrepostos" ~: 
    [Personagem (1, 2) 7, Personagem (4, 5) 6, Personagem (8, 9) 10]
    ~=? juncaoMapas [Personagem (1, 2) 3, Personagem (4, 5) 6] [Personagem (1, 2) 7, Personagem (8, 9) 10]]