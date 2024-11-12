type Coordenada = (Float, Float)

type Comprimento = Float

type Largura = Float

data Movimento = E | W | N | S deriving (Show, Eq)

type Mapa = (Comprimento, Largura)

-- Questão 9
sequenciaMovimentos :: Mapa -> Coordenada -> [Movimento]
sequenciaMovimentos (comprimento, largura) (x, y)
  | x < comprimento / 2 && y < largura / 2 = replicate (floor x) W ++ replicate (floor y) S
  | x >= comprimento / 2 && y < largura / 2 = replicate (floor (comprimento - x)) E ++ replicate (floor y) S
  | x < comprimento / 2 && y >= largura / 2 = replicate (floor x) W ++ replicate (floor (largura - y)) N
  | otherwise = replicate (floor (comprimento - x)) E ++ replicate (floor (largura - y)) N



teste1 = sequenciaMovimentos (10, 8) (2, 3)
teste2 = sequenciaMovimentos (10, 8) (8, 5)

-- Questão 10
data Personagem = Personagem Coordenada Int deriving (Show, Eq)

juncaoMapas :: [Personagem] -> [Personagem] -> [Personagem]
juncaoMapas mapa1 mapa2 = undefined  -- Implementar a lógica de junção dos mapas

teste3 = juncaoMapas [Personagem (1, 2) 3, Personagem (4, 5) 6] [Personagem (1, 2) 7, Personagem (8, 9) 10]
teste4 = juncaoMapas [Personagem (1, 2) 3, Personagem (4, 5) 6] [Personagem (7, 8) 9, Personagem (10, 11) 12]